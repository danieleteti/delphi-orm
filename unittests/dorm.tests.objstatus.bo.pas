{ *******************************************************************************
  Copyright 2010-2013 Daniele Teti

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

unit dorm.tests.objstatus.bo;

interface

uses
  dorm,
  dorm.Commons,
  Classes,
  Generics.Collections,
  dorm.Mappings, dorm.ObjectStatus;

type
  TObjStatusSupport = class
  strict protected
    FHistory: TStringList;
    FObjStatus: TdormObjectStatus;
    procedure SetObjStatus(const Value: TdormObjectStatus);
    procedure AddhistoryEvent(const Value: String);

  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetEventAndValidationHistory: TStringList;
    procedure Validate;
    procedure DeleteValidate;
    procedure InsertValidate;
    procedure UpdateValidate;
    procedure OnAfterDelete;
    procedure OnAfterInsert;
    procedure OnAfterLoad;
    procedure OnAfterPersist;
    procedure OnAfterUpdate;
    procedure OnBeforeDelete;
    procedure OnBeforeInsert;
    procedure OnBeforeLoad;
    procedure OnBeforePersist;
    procedure OnBeforeUpdate;

  public
    [Transient]
    property objstatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

  TPersonOS = class;
  TPhoneOS = class;
  TEmployeeOS = class;

  [ListOf('TPhoneOS')]
  TPhonesOS = class(

{$IF CompilerVersion >= 23}

    TObjectList<TPhoneOS>

{$ELSE}

    TdormObjectList<TPhoneOS>

{$IFEND}

    )
  end;

  [Entity('cars')]
  TCarOS = class(TObjStatusSupport)
  private
    FModel: string;
    FBrand: string;
    FPersonID: Integer;
    FID: Integer;
    FOwner: TPersonOS;
    procedure SetBrand(const Value: string);
    procedure SetModel(const Value: string);
    procedure SetPersonID(const Value: Integer);
    procedure SetID(const Value: Integer);
    procedure SetOwner(const Value: TPersonOS);
    // Private!!!
    [Column('ID_PERSON')]
    property PersonID: Integer read FPersonID write SetPersonID;

  public
    class function NewCar: TCarOS;
    property ID: Integer read FID write SetID;
    property Brand: string read FBrand write SetBrand;
    property Model: string read FModel write SetModel;
    property Owner: TPersonOS read FOwner write SetOwner;
  end;

  [Entity('emails')]
  TEmailOS = class(TObjStatusSupport)
  private
    FValue: string;
    FPersonID: Integer;
    FID: Integer;
    FCopiedValue: String; // used only for the test
    procedure SetValue(const Value: string);
    procedure SetPersonID(const Value: Integer);
    procedure SetID(const Value: Integer);
    // Private!!!
    [Column('ID_PERSON')]
    property PersonID: Integer read FPersonID write SetPersonID;

  public
    class function NewEmail: TEmailOS;
    procedure Validate;
    procedure OnAfterLoad;
    property ID: Integer read FID write SetID;
    [Column('ADDRESS')]
    property Value: string read FValue write SetValue;
    [Transient]
    property CopiedValue: String read FCopiedValue write FCopiedValue;
  end;

  [Entity('people')]
  TPersonOS = class(TObjStatusSupport)
  private
    FLastName: string;
    FAge: Int32;
    FFirstName: string;
    FID: Integer;
    FBornDate: TDate;
    FPhones: TPhonesOS;
    FCar: TCarOS;
    FEmail: TEmailOS;
    FBornTimeStamp: TDateTime;
    FPhoto: TStream;
    FIsMale: Boolean;
    procedure SetLastName(const Value: string);
    procedure SetAge(const Value: Int32);
    procedure SetFirstName(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetBornDate(const Value: TDate);
    procedure SetCar(const Value: TCarOS);
    procedure SetEmail(const Value: TEmailOS);
    procedure SetBornTimeStamp(const Value: TDateTime);
    procedure SetPhoto(const Value: TStream);
    function GetFullName: string;
    procedure SetIsMale(const Value: Boolean);

  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    function ToString: string; override;
    class function NewPerson: TPersonOS;
    property ID: Integer read FID write SetID;
    [Column('FIRST_NAME')]
    property FirstName: string read FFirstName write SetFirstName;
    [Column('LAST_NAME')]
    property LastName: string read FLastName write SetLastName;
    property Age: Int32 read FAge write SetAge;
    [Column('BORN_DATE')]
    property BornDate: TDate read FBornDate write SetBornDate;
    [Column('BORN_DATE_TIME')]
    property BornTimeStamp: TDateTime read FBornTimeStamp
      write SetBornTimeStamp;
    [HasMany('PersonID')]
    property Phones: TPhonesOS read FPhones;
    [HasOne('PersonID')]
    property Car: TCarOS read FCar write SetCar;
    [HasOne('PersonID')]
    property Email: TEmailOS read FEmail write SetEmail;
    property Photo: TStream read FPhoto write SetPhoto;
    [Column('IS_MALE')]
    property IsMale: Boolean read FIsMale write SetIsMale;
    [Transient]
    property FullName: string read GetFullName;

  end;

  [ListOf('TPersonOS')]
  TPeopleOS = class(TObjectList<TPersonOS>)
  end;

  [Entity('phones')]
  TPhoneOS = class(TObjStatusSupport)
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
    [Column('ID_PERSON')]
    property PersonID: Integer read FPersonID write SetPersonID;

  public
    class constructor Create;
    class procedure register;
    constructor Create; override;
    property Number: string read FNumber write SetNumber;
    property Model: string read FModel write SetModel;
    property ID: Integer read FID write SetID;
    [NoAutomapping]
    property IsItalianNumber: Boolean read GetIsItalianNumber;
  end;

  [ListOf('TEmployeeOS')]
  TEmployeesOS = class
    ({$IF CompilerVersion >= 23}TObjectList<TEmployeeOS>{$ELSE}TdormObjectList<TEmployeeOS>{$IFEND})
  end;

  [Entity('departments')]
  TDepartmentOS = class(TObjStatusSupport)
  private
    FDepartmentName: string;
    FID: string;
    FEmployees: TEmployeesOS;
    procedure setDepartmentName(const Value: string);
    procedure setEmployees(const Value: TEmployeesOS);
    procedure SetID(const Value: string);

  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    class function NewDepartment(DepName: string): TDepartmentOS;
    property ID: string read FID write SetID;
    property DepartmentName: string read FDepartmentName
      write setDepartmentName;
    property Employees: TEmployeesOS read FEmployees write setEmployees;
  end;

  [Entity('employee')]
  TEmployeeOS = class(TObjStatusSupport)
  private
    FLastName: string;
    FEmployeeID: string;
    FAddress: string;
    FFirstName: string;
    FDepartment: TDepartmentOS;
    FDepartmentID: string;
    procedure setAddress(const Value: string);
    procedure setDepartment(const Value: TDepartmentOS);
    procedure setEmployeeID(const Value: string);
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    property DepartmentID: string read FDepartmentID write FDepartmentID;

  public
    class function NewEmployee(): TEmployeeOS;
    property EmployeeID: string read FEmployeeID write setEmployeeID;
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Address: string read FAddress write setAddress;
    property Department: TDepartmentOS read FDepartment write setDepartment;
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

constructor TPersonOS.Create;
begin
  inherited;
  FPhoto := nil;
  FPhones := TPhonesOS.Create(true);
  FCar := TCarOS.Create;
  FEmail := TEmailOS.Create;
  FEmail.Value := 'd.teti@bittime.it';
end;

destructor TPersonOS.Destroy;
begin
  FreeAndNil(FPhoto);
  FreeAndNil(FPhones);
  if Assigned(FCar) then
    FreeAndNil(FCar);
  if Assigned(FEmail) then
    FreeAndNil(FEmail);
  inherited;
end;

function TPersonOS.GetFullName: string;
begin
  Result := FirstName + ' ' + LastName;
end;

class function TPersonOS.NewPerson: TPersonOS;
begin
  Result := TPersonOS.Create;
  Result.FirstName := 'Daniele';
  Result.LastName := 'Teti';
  Result.Age := 32;
  Result.BornDate := EncodeDate(1979, 11, 4);
  Result.BornTimeStamp := EncodeDateTime(1979, 11, 4, 16, 10, 00, 0);
end;

procedure TPersonOS.SetCar(const Value: TCarOS);
begin
  FCar := Value;
end;

procedure TPersonOS.SetEmail(const Value: TEmailOS);
begin
  FEmail := Value;
end;

procedure TPersonOS.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TPersonOS.SetBornDate(const Value: TDate);
begin
  FBornDate := Value;
end;

procedure TPersonOS.SetBornTimeStamp(const Value: TDateTime);
begin
  FBornTimeStamp := Value;
end;

procedure TPersonOS.SetAge(const Value: Int32);
begin
  FAge := Value;
end;

procedure TPersonOS.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPersonOS.SetIsMale(const Value: Boolean);
begin
  FIsMale := Value;
end;

procedure TPersonOS.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPersonOS.SetPhoto(const Value: TStream);
begin
  FPhoto := Value;
end;

function TPersonOS.ToString: string;
begin
  Result := Format('ID: %d, Nome: %s, Cognome: %s, Età: %d nato il %s',
    [Self.ID, Self.FirstName, Self.LastName, Self.Age,
    datetostr(Self.BornDate)]);
end;

{ TPhone }

class constructor TPhoneOS.Create;
begin
  // do nothing
end;

{ todo: "We need to support object with many constructors. Obviously, the default one is needed" }
constructor TPhoneOS.Create;
begin
  inherited;
end;

function TPhoneOS.GetIsItalianNumber: Boolean;
begin
  Result := (Copy(Self.Number, 1, 3) = '+39') or
    (Copy(Self.Number, 1, 4) = '0039');
end;

class procedure TPhoneOS.register;
begin

end;

procedure TPhoneOS.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPhoneOS.SetNumber(const Value: string);
begin
  FNumber := Value;
end;

procedure TPhoneOS.SetPersonID(const Value: Integer);
begin
  FPersonID := Value;
end;

procedure TPhoneOS.SetModel(const Value: string);
begin
  FModel := Value;
end;

{ TCar }

class function TCarOS.NewCar: TCarOS;
begin
  Result := TCarOS.Create;
  Result.Brand := 'Ford';
  Result.Model := 'Fosuc 1.8 TDCi';
end;

procedure TCarOS.SetBrand(const Value: string);
begin
  FBrand := Value;
end;

procedure TCarOS.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TCarOS.SetModel(const Value: string);
begin
  FModel := Value;
end;

procedure TCarOS.SetOwner(const Value: TPersonOS);
begin
  FOwner := Value;
  Self.FPersonID := FOwner.ID;
end;

procedure TCarOS.SetPersonID(const Value: Integer);
begin
  FPersonID := Value;
end;

{ TEmail }

class function TEmailOS.NewEmail: TEmailOS;
begin
  Result := TEmailOS.Create;
  Result.Value := 'd.teti@bittime.it';
end;

procedure TEmailOS.OnAfterLoad;
begin
  inherited;
  CopiedValue := UpperCase(Value);
end;

procedure TEmailOS.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TEmailOS.SetPersonID(const Value: Integer);
begin
  FPersonID := Value;
end;

procedure TEmailOS.SetValue(const Value: string);
begin
  FValue := Value;
end;

procedure TEmailOS.Validate;
begin
  if not IsValidEmail(Value) then
    raise EdormValidationException.Create('Invalid email');
end;

{ TDepartment }

constructor TDepartmentOS.Create;
begin
  inherited;
  FEmployees := TEmployeesOS.Create();
end;

destructor TDepartmentOS.Destroy;
begin
  FEmployees.Free;
  inherited;
end;

class function TDepartmentOS.NewDepartment(DepName: string): TDepartmentOS;
begin
  Result := TDepartmentOS.Create();
  Result.DepartmentName := DepName;
end;

procedure TDepartmentOS.setDepartmentName(const Value: string);
begin
  FDepartmentName := Value;
end;

procedure TDepartmentOS.setEmployees(const Value: TEmployeesOS);
begin
  FEmployees := Value;
end;

procedure TDepartmentOS.SetID(const Value: string);
begin
  FID := Value;
end;

{ TEmployee }

class function TEmployeeOS.NewEmployee: TEmployeeOS;
begin
  Result := TEmployeeOS.Create;
  Result.FirstName := 'Jack';
  Result.LastName := 'Sheppard';
  Result.Address := 'Perdido en una isla desierta';
end;

procedure TEmployeeOS.setAddress(const Value: string);
begin
  FAddress := Value;
end;

procedure TEmployeeOS.setDepartment(const Value: TDepartmentOS);
begin
  FDepartment := Value;
end;

procedure TEmployeeOS.setEmployeeID(const Value: string);
begin
  FEmployeeID := Value;
end;

procedure TEmployeeOS.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TEmployeeOS.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TObjStatusSupport.AddhistoryEvent(const Value: String);
begin
  FHistory.Add(ClassName + '.' + Value);
end;

constructor TObjStatusSupport.Create;
begin
  inherited;
  FHistory := TStringList.Create;
end;

procedure TObjStatusSupport.DeleteValidate;
begin
  AddhistoryEvent('DeleteValidate')
end;

destructor TObjStatusSupport.Destroy;
begin
  FHistory.Free;
  inherited;
end;

function TObjStatusSupport.GetEventAndValidationHistory: TStringList;
begin
  FHistory.Delimiter := '-';
  Result := FHistory;
end;

procedure TObjStatusSupport.InsertValidate;
begin
  AddhistoryEvent('InsertValidate')
end;

procedure TObjStatusSupport.OnAfterDelete;
begin
  AddhistoryEvent('OnAfterDelete')
end;

procedure TObjStatusSupport.OnAfterInsert;
begin
  AddhistoryEvent('OnAfterInsert')
end;

procedure TObjStatusSupport.OnAfterLoad;
begin
  AddhistoryEvent('OnAfterLoad')
end;

procedure TObjStatusSupport.OnAfterPersist;
begin
  AddhistoryEvent('OnAfterPersist')
end;

procedure TObjStatusSupport.OnAfterUpdate;
begin
  AddhistoryEvent('OnAfterUpdate')
end;

procedure TObjStatusSupport.OnBeforeDelete;
begin
  AddhistoryEvent('OnBeforeDelete')
end;

procedure TObjStatusSupport.OnBeforeInsert;
begin
  AddhistoryEvent('OnBeforeInsert')
end;

procedure TObjStatusSupport.OnBeforeLoad;
begin
  AddhistoryEvent('OnBeforeLoad')
end;

procedure TObjStatusSupport.OnBeforePersist;
begin
  AddhistoryEvent('OnBeforePersist')
end;

procedure TObjStatusSupport.OnBeforeUpdate;
begin
  AddhistoryEvent('OnBeforeUpdate')
end;

procedure TObjStatusSupport.UpdateValidate;
begin
  AddhistoryEvent('UpdateValidate')
end;

procedure TObjStatusSupport.Validate;
begin
  AddhistoryEvent('Validate')
end;

procedure TObjStatusSupport.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

end.
