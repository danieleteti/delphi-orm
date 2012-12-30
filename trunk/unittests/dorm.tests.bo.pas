{ *******************************************************************************
  Copyright 2010-2012 Daniele Teti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ******************************************************************************** }

unit dorm.tests.bo;

{$RTTI EXPLICIT
  FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])
  METHODS([vcPrivate, vcProtected, vcPublic, vcPublished])
  PROPERTIES([vcPrivate, vcProtected, vcPublic, vcPublished])}

interface

uses
  dorm,
  dorm.Commons,
  Classes,
  Generics.Collections,
  dorm.Mappings;

type
  TPerson = class;
  TPhone = class;
  TEmployee = class;

  TPhones = class(
{$IF CompilerVersion >= 23}
    TObjectList<TPhone>
{$ELSE}
    TdormObjectList<TPhone>
{$IFEND}
    )
  end;

  TCar = class
  private
    FModel: string;
    FBrand: string;
    FPersonID: Integer;
    FID: Integer;
    FOwner: TPerson;
    procedure SetBrand(const Value: string);
    procedure SetModel(const Value: string);
    procedure SetPersonID(const Value: Integer);
    procedure SetID(const Value: Integer);
    procedure SetOwner(const Value: TPerson);
    // Private!!!
    property PersonID: Integer read FPersonID write SetPersonID;
  public
    class function NewCar: TCar;
    property ID: Integer read FID write SetID;
    property Brand: string read FBrand write SetBrand;
    property Model: string read FModel write SetModel;
    property Owner: TPerson read FOwner write SetOwner;
  end;

  TEmail = class(TdormObject)
  private
    FValue: string;
    FPersonID: Integer;
    FID: Integer;
    FCopiedValue: String; // used only for the test
    procedure SetValue(const Value: string);
    procedure SetPersonID(const Value: Integer);
    procedure SetID(const Value: Integer);
    // Private!!!
    property PersonID: Integer read FPersonID write SetPersonID;
  public
    class function NewEmail: TEmail;
    function Validate: Boolean; override;
    procedure OnAfterLoad; override;
    property ID: Integer read FID write SetID;
    property Value: string read FValue write SetValue;
    [Transient]
    property CopiedValue: String read FCopiedValue write FCopiedValue;
  end;

  TPerson = class
  private
    FLastName: string;
    FAge: Int32;
    FFirstName: string;
    FID: Integer;
    FBornDate: TDate;
    FPhones: TPhones;
    FCar: TCar;
    FEmail: TEmail;
    FBornTimeStamp: TDateTime;
    FPhoto: TStream;
    FIsMale: Boolean;
    procedure SetLastName(const Value: string);
    procedure SetAge(const Value: Int32);
    procedure SetFirstName(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetBornDate(const Value: TDate);
    procedure SetCar(const Value: TCar);
    procedure SetEmail(const Value: TEmail);
    procedure SetBornTimeStamp(const Value: TDateTime);
    procedure SetPhoto(const Value: TStream);
    function GetFullName: string;
    procedure SetIsMale(const Value: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function ToString: string; override;
    class function NewPerson: TPerson;
    property ID: Integer read FID write SetID;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Age: Int32 read FAge write SetAge;
    property BornDate: TDate read FBornDate write SetBornDate;
    property BornTimeStamp: TDateTime read FBornTimeStamp
      write SetBornTimeStamp;
    property Phones: TPhones read FPhones;
    property Car: TCar read FCar write SetCar;
    property Email: TEmail read FEmail write SetEmail;
    property Photo: TStream read FPhoto write SetPhoto;
    property IsMale: Boolean read FIsMale write SetIsMale;
    [Transient]
    property FullName: string read GetFullName;
  end;

  TPhone = class
  private
    FNumber: string;
    FModel: string;
    FID: Integer;
    FPersonID: Integer;
    procedure SetNumber(const Value: string);
    procedure SetModel(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetPersonID(const Value: Integer);
    function GetIsItalianNumber: Boolean;
    // Private!!!
    property PersonID: Integer read FPersonID write SetPersonID;
  public
    class constructor Create;
    class procedure register;
    constructor Create;
    property Number: string read FNumber write SetNumber;
    property Model: string read FModel write SetModel;
    property ID: Integer read FID write SetID;
    [NoAutomapping]
    property IsItalianNumber: Boolean read GetIsItalianNumber;
  end;

  TEmployees = class({$IF CompilerVersion >= 23}TObjectList<TEmployee>{$ELSE}TdormObjectList<TEmployee>{$IFEND})
  end;

  TDepartment = class
  private
    FDepartmentName: string;
    FID: string;
    FEmployees: TEmployees;
    procedure setDepartmentName(const Value: string);
    procedure setEmployees(const Value: TEmployees);
    procedure SetID(const Value: string);
  public
    constructor Create(); virtual;
    destructor Destroy; override;
    class function NewDepartment(DepName: string): TDepartment;
    property ID: string read FID write SetID;
    property DepartmentName: string read FDepartmentName
      write setDepartmentName;
    property Employees: TEmployees read FEmployees write setEmployees;
  end;

  TEmployee = class
  private
    FLastName: string;
    FEmployeeID: string;
    FAddress: string;
    FFirstName: string;
    FDepartment: TDepartment;
    FDepartmentID: string;
    procedure setAddress(const Value: string);
    procedure setDepartment(const Value: TDepartment);
    procedure setEmployeeID(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    property DepartmentID: string read FDepartmentID write FDepartmentID;
  public
    class function NewEmployee(): TEmployee;
    property EmployeeID: string read FEmployeeID write setEmployeeID;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Address: string read FAddress write setAddress;
    property Department: TDepartment read FDepartment write setDepartment;
  end;

  [Entity('PEOPLE')]
  TPersonDirty = class(TPerson)
  private
    FObjStatus: TdormObjectStatus;
    procedure SetObjStatus(const Value: TdormObjectStatus);
  public
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

  TPhoneDirty = class(TPhone)
  private
    FObjStatus: TdormObjectStatus;
    procedure SetObjStatus(const Value: TdormObjectStatus);
  public
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

  TCarDirty = class(TCar)
  private
    FObjStatus: TdormObjectStatus;
    procedure SetObjStatus(const Value: TdormObjectStatus);
  public
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

  TEmailDirty = class(TEmail)
  private
    FObjStatus: TdormObjectStatus;
    procedure SetObjStatus(const Value: TdormObjectStatus);
  public
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

implementation

uses
  SysUtils,
  DateUtils;

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
    Result := true;
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

{ TPerson }

constructor TPerson.Create;
begin
  inherited;
  FPhoto := nil;
  FPhones := TPhones.Create(true);
  FCar := TCar.Create;
  FEmail := TEmail.Create;
  FEmail.Value := 'd.teti@bittime.it';
end;

destructor TPerson.Destroy;
begin
  FreeAndNil(FPhoto);
  FreeAndNil(FPhones);
  if assigned(FCar) then
    FreeAndNil(FCar);
  if assigned(FEmail) then
    FreeAndNil(FEmail);
  inherited;
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
  Result.Age := 32;
  Result.BornDate := EncodeDate(1979, 11, 4);
  Result.BornTimeStamp := EncodeDateTime(1979, 11, 4, 16, 10, 00, 0);
end;

procedure TPerson.SetCar(const Value: TCar);
begin
  FCar := Value;
end;

procedure TPerson.SetEmail(const Value: TEmail);
begin
  FEmail := Value;
end;

procedure TPerson.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TPerson.SetBornDate(const Value: TDate);
begin
  FBornDate := Value;
end;

procedure TPerson.SetBornTimeStamp(const Value: TDateTime);
begin
  FBornTimeStamp := Value;
end;

procedure TPerson.SetAge(const Value: Int32);
begin
  FAge := Value;
end;

procedure TPerson.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPerson.SetIsMale(const Value: Boolean);
begin
  FIsMale := Value;
end;

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetPhoto(const Value: TStream);
begin
  FPhoto := Value;
end;

function TPerson.ToString: string;
begin
  Result := Format('ID: %d, Nome: %s, Cognome: %s, Età: %d nato il %s',
    [Self.ID, Self.FirstName, Self.LastName, Self.Age,
    datetostr(Self.BornDate)]);
end;

{ TPhone }

class constructor TPhone.Create;
begin
  // do nothing
end;

{ todo: "We need to support object with many constructors. Obviously, the default one is needed" }
constructor TPhone.Create;
begin
  inherited;
end;

function TPhone.GetIsItalianNumber: Boolean;
begin
  Result := (Copy(Self.Number, 1, 3) = '+39') or
    (Copy(Self.Number, 1, 4) = '0039');
end;

class procedure TPhone.register;
begin

end;

procedure TPhone.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPhone.SetNumber(const Value: string);
begin
  FNumber := Value;
end;

procedure TPhone.SetPersonID(const Value: Integer);
begin
  FPersonID := Value;
end;

procedure TPhone.SetModel(const Value: string);
begin
  FModel := Value;
end;

{ TCar }

class function TCar.NewCar: TCar;
begin
  Result := TCar.Create;
  Result.Brand := 'Ford';
  Result.Model := 'Fosuc 1.8 TDCi';
end;

procedure TCar.SetBrand(const Value: string);
begin
  FBrand := Value;
end;

procedure TCar.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TCar.SetModel(const Value: string);
begin
  FModel := Value;
end;

procedure TCar.SetOwner(const Value: TPerson);
begin
  FOwner := Value;
  Self.FPersonID := FOwner.ID;
end;

procedure TCar.SetPersonID(const Value: Integer);
begin
  FPersonID := Value;
end;

{ TEmail }

class function TEmail.NewEmail: TEmail;
begin
  Result := TEmail.Create;
  Result.Value := 'd.teti@bittime.it';
end;

procedure TEmail.OnAfterLoad;
begin
  inherited;
  CopiedValue := UpperCase(Value);
end;

procedure TEmail.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TEmail.SetPersonID(const Value: Integer);
begin
  FPersonID := Value;
end;

procedure TEmail.SetValue(const Value: string);
begin
  FValue := Value;
end;

function TEmail.Validate: Boolean;
begin
  Result := IsValidEmail(Value);
  if not Result then
    AddError('Invalid email');
end;

{ TDepartment }

constructor TDepartment.Create;
begin
  inherited;
  FEmployees := TEmployees.Create();
end;

destructor TDepartment.Destroy;
begin
  FEmployees.Free;
  inherited;
end;

class function TDepartment.NewDepartment(DepName: string): TDepartment;
begin
  Result := TDepartment.Create();
  Result.DepartmentName := DepName;
end;

procedure TDepartment.setDepartmentName(const Value: string);
begin
  FDepartmentName := Value;
end;

procedure TDepartment.setEmployees(const Value: TEmployees);
begin
  FEmployees := Value;
end;

procedure TDepartment.SetID(const Value: string);
begin
  FID := Value;
end;

{ TEmployee }

class function TEmployee.NewEmployee: TEmployee;
begin
  Result := TEmployee.Create;
  Result.FirstName := 'Jack';
  Result.LastName := 'Sheppard';
  Result.Address := 'Perdido en una isla desierta';
end;

procedure TEmployee.setAddress(const Value: string);
begin
  FAddress := Value;
end;

procedure TEmployee.setDepartment(const Value: TDepartment);
begin
  FDepartment := Value;
end;

procedure TEmployee.setEmployeeID(const Value: string);
begin
  FEmployeeID := Value;
end;

procedure TEmployee.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TEmployee.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

{ TPersonDirty }

procedure TPersonDirty.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

{ TPhoneDirty }

procedure TPhoneDirty.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

{ TCarDirty }

procedure TCarDirty.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

{ TEmailDirty }

procedure TEmailDirty.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

end.
