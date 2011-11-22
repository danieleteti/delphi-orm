{ *******************************************************************************
  Copyright 2010-2011 Daniele Teti

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

{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished]) METHODS([vcPrivate, vcProtected, vcPublic, vcPublished]) PROPERTIES([vcPrivate, vcProtected, vcPublic, vcPublished])}

interface

uses
  dorm.Commons, dorm.Collections, dorm.InterposedObject,
  {$IFDEF VER210}System.{$ENDIF}Classes;

type
  TPerson = class;

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
    FValue: String;
    FPersonID: Integer;
    FID: Integer;
    procedure SetValue(const Value: String);
    procedure SetPersonID(const Value: Integer);
    procedure SetID(const Value: Integer);
    // Private!!!
    property PersonID: Integer read FPersonID write SetPersonID;
  public
    class function NewEmail: TEmail;
    function Validate: Boolean; override;
    property ID: Integer read FID write SetID;
    property Value: String read FValue write SetValue;
  end;

  TPerson = class
  private
    FLastName: string;
    FAge: Int32;
    FFirstName: string;
    FID: Integer;
    FBornDate: TDate;
    FPhones: TdormCollection;
    FCar: TCar;
    FEmail: TEmail;
    FBornTimeStamp: TDateTime;
    FPhoto: TStream;
    procedure SetLastName(const Value: string);
    procedure SetAge(const Value: Int32);
    procedure SetFirstName(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetBornDate(const Value: TDate);
    procedure SetPhones(const Value: TdormCollection);
    procedure SetCar(const Value: TCar);
    procedure SetEmail(const Value: TEmail);
    procedure SetBornTimeStamp(const Value: TDateTime);
    procedure SetPhoto(const Value: TStream);
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
    property Phones: TdormCollection read FPhones write SetPhones;
    property Car: TCar read FCar write SetCar;
    property Email: TEmail read FEmail write SetEmail;
    property Photo: TStream read FPhoto write SetPhoto;
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
    // Private!!!
    property PersonID: Integer read FPersonID write SetPersonID;
  public
    class constructor Create;
    class procedure Register;
    constructor Create;
  published
    property Number: string read FNumber write SetNumber;
    property Model: string read FModel write SetModel;
    property ID: Integer read FID write SetID;
  end;

implementation

uses
  SysUtils, DateUtils;

function IsValidEmail(const Value: string): Boolean;

  function CheckAllowed(const s: string): Boolean;
  var
    i: Integer;
  begin
    Result := false;
    for i := 1 to Length(s) do
      if not(s[i] in ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_', '-', '.']) then
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
  FPhones := nil;
  FCar := nil;
end;

destructor TPerson.Destroy;
begin
  FreeAndNil(FPhoto);
  FreeAndNil(FPhones);
  FreeAndNil(FCar);
  FreeAndNil(FEmail);
  inherited;
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

procedure TPerson.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TPerson.SetPhones(const Value: TdormCollection);
begin
  FPhones := Value;
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

class procedure TPhone.Register;
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

procedure TEmail.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TEmail.SetPersonID(const Value: Integer);
begin
  FPersonID := Value;
end;

procedure TEmail.SetValue(const Value: String);
begin
  FValue := Value;
end;

function TEmail.Validate: Boolean;
begin
  Result := IsValidEmail(Value);
  if not Result then
    AddError('Invalid email');
end;

end.
