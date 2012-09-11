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

unit TestDORMRelations;

interface

uses
  TestFramework,
  TypInfo,
  dorm,
  dorm.Commons,
  BaseTestCase;

type
  TTestDORMRelations = class(TBaseTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadHasMany;
    procedure TestHasManyLazyLoad;
    procedure TestBelongsTo;
    procedure TestLoadRelations;
  end;

implementation

uses
  Rtti,
  Classes,
  dorm.Collections,
  dorm.tests.bo,
  SysUtils;

{ TTestDORMRelations }

procedure TTestDORMRelations.SetUp;
begin
  inherited;
end;

procedure TTestDORMRelations.TearDown;
begin
  inherited;
end;

procedure TTestDORMRelations.TestBelongsTo;
var
  Car: TCar;
  CarOwner: TPerson;
  car_id: Integer;
begin
  CarOwner := TPerson.NewPerson;
  try
    Session.Insert(CarOwner);
    Car := CarOwner.Car;
    Car.Brand := 'Ford';
    Car.Model := 'Focus 1.8 TDCi';
    Car.Owner := CarOwner;
    Session.Update(Car);
    car_id := Car.ID;
  finally
    CarOwner.Free;
  end;

  // LazyLoaded is enabled in the conf file
  Car := Session.Load<TCar>(car_id);
  try
    CheckNull(Car.Owner);
    Session.LoadRelations(Car);
    CheckNull(Car.Owner);
    Session.DisableLazyLoad(TCar, 'Owner');
    // Session.SetLazyLoadFor(TypeInfo(TCar), 'Owner', false);
    { todo: check this }
    Session.LoadRelations(Car);
    CheckNotNull(Car.Owner);
  finally
    Car.Owner.Free;
    Car.Free;
  end;

  // LazyLoaded is enabled in the conf file
  Session.DisableLazyLoad(TCar, 'Owner');
  Session.DisableLazyLoad(TPerson, 'Car');
  Car := Session.Load<TCar>(car_id);
  try
    CheckTrue(Assigned(Car.Owner));
    CheckEquals('Daniele', Car.Owner.FirstName);
    CheckEquals('Teti', Car.Owner.LastName);
    CheckEquals(32, Car.Owner.Age);
    CheckEquals('d.teti@bittime.it', Car.Owner.Email.Value);
  finally
    Car.Owner.Free;
    Car.Free;
  end;
end;

procedure TTestDORMRelations.TestHasManyLazyLoad;
var
  p: TPerson;
  // t: TPhone;
  ID: Integer;
begin
  p := TPerson.NewPerson;
  try
    p.Phones.Add(TPhone.Create);
    Session.Insert(p);
    ID := p.ID;
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  // Now Test with lazy load ON
  Session.EnableLazyLoad(TPerson, 'Phones');
  // Session.SetLazyLoadFor(TypeInfo(TPerson), 'Phones', true);
  p := Session.Load<TPerson>(ID);
  try
    CheckEquals(0, p.Phones.Count);
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  // Test with lazy load OFF
  Session.DisableLazyLoad(TPerson, 'Phones');
  p := Session.Load<TPerson>(ID);
  try
    CheckEquals(1, p.Phones.Count); // Child objects are loaded
    Session.Commit;
  finally
    p.Free;
  end;
end;

procedure TTestDORMRelations.TestLoadHasMany;
var
  t, t1: TPhone;
  p: TPerson;
  ID: Integer;
begin
  p := TPerson.NewPerson;
  try
    p.Email.Value := 'hello@there.com';
    t := TPhone.Create;
    t.Number := '555-7765123';
    t.Model := 'Casa';
    p.Phones.Add(t);
    t1 := TPhone.Create;
    t1.Number := '555-7765123';
    t1.Model := 'Casa';
    p.Phones.Add(t1);
    p.Car.Brand := 'Ford';
    p.Car.Model := 'Focus 1.8 TDCi';
    Session.Insert(p); // save Person, telefoni and Car
    ID := p.ID;
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  p := Session.Load<TPerson>(ID);
  try
    CheckEquals(2, p.Phones.Count);
    CheckEquals('Ford', p.Car.Brand);
    CheckEquals('Focus 1.8 TDCi', p.Car.Model);
    CheckEquals(UpperCase(p.Email.Value), p.Email.CopiedValue);
    Session.Commit;
  finally
    p.Free;
  end;
end;

procedure TTestDORMRelations.TestLoadRelations;
var
  p: TPerson;
  oid: Integer;
begin
  Session.EnableLazyLoad(TPerson, 'Car');
  Session.EnableLazyLoad(TPerson, 'Phones');
  Session.EnableLazyLoad(TPerson, 'Email');
  p := TPerson.NewPerson;
  try
    Session.Insert(p);
    oid := p.ID;
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(oid);
  try
    Session.DisableLazyLoad(TPerson, 'Car');
    Session.DisableLazyLoad(TPerson, 'Phones');
    Session.DisableLazyLoad(TPerson, 'Email');
    CheckFalse(Session.OIDIsSet(p.Car));
    CheckEquals(0, p.Phones.Count);
    CheckFalse(Session.OIDIsSet(p.Email));
    Session.LoadRelations(p, [drBelongsTo]);
    CheckFalse(Session.OIDIsSet(p.Car));
    CheckEquals(0, p.Phones.Count);
    CheckFalse(Session.OIDIsSet(p.Email));
    Session.LoadRelations(p, [drHasMany]);
    CheckFalse(Session.OIDIsSet(p.Car));
    CheckEquals(0, p.Phones.Count);
    CheckFalse(Session.OIDIsSet(p.Email));
    Session.LoadRelations(p, [drHasOne]);
    CheckTrue(Session.OIDIsSet(p.Car));
    CheckEquals(0, p.Phones.Count);
    CheckTrue(Session.OIDIsSet(p.Email));
  finally
    p.Free;
  end;

end;

initialization

RegisterTest(TTestDORMRelations.Suite);

finalization

end.
