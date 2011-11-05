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

unit TestDORMRelations;

interface

uses
  TestFramework,
  TypInfo,
  dorm,
  dorm.Commons, BaseTestCase;

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
  dorm.Collections, dorm.tests.bo;

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
  Exit;
  CarOwner := TPerson.NewPerson;
  try
    Session.Save(CarOwner);
    Car := TCar.Create;
    try
      Car.Brand := 'Ford';
      Car.Model := 'Focus 1.8 TDCi';
      Car.Owner := CarOwner;
      Session.Save(Car);
      car_id := Car.ID;
    finally
      Car.Free;
    end;
  finally
    CarOwner.Free;
  end;

  // LazyLoaded is enabled in the conf file
  Car := Session.Load<TCar>(car_id);
  try
    CheckNull(Car.Owner);
    Session.LoadRelations(Car);
    CheckNull(Car.Owner);
    Session.SetLazyLoadFor(TypeInfo(TCar), 'Owner', false);
    { todo: This still not works!!! }
    Session.LoadRelations(Car);
    CheckNotNull(Car.Owner);
  finally
    Car.Owner.Free;
    Car.Free;
  end;

end;

procedure TTestDORMRelations.TestHasManyLazyLoad;
var
  p: TPerson;
  t: TPhone;
  ID: Integer;
begin
  p := TPerson.NewPerson;
  try
    p.Phones.Add(TPhone.Create);
    Session.Save(p);
    ID := p.ID;
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  // Now Test with lazy load ON
  Session.SetLazyLoadFor(TypeInfo(TPerson), 'Phones', true);
  p := Session.Load<TPerson>(ID);
  try
    CheckEquals(0, p.Phones.Count);
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  // Test with lazy load OFF
  Session.SetLazyLoadFor(TypeInfo(TPerson), 'Phones', false);
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
    p.Phones := NewList;
    t := TPhone.Create;
    t.Number := '555-7765123';
    t.Model := 'Casa';
    p.Phones.Add(t);
    t1 := TPhone.Create;
    t1.Number := '555-7765123';
    t1.Model := 'Casa';
    p.Phones.Add(t1);
    p.Car := TCar.Create;
    p.Car.Brand := 'Ford';
    p.Car.Model := 'Focus 1.8 TDCi';
    Session.Save(p); // save Person and telefoni
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
  Session.SetLazyLoadFor(TypeInfo(TPerson), 'Car', true);
  Session.SetLazyLoadFor(TypeInfo(TPerson), 'Phones', true);
  Session.SetLazyLoadFor(TypeInfo(TPerson), 'Email', true);
  p := TPerson.NewPerson;
  try
    Session.Save(p);
    oid := p.ID;
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(oid);
  try
    Session.SetLazyLoadFor(TypeInfo(TPerson), 'Car', false);
    Session.SetLazyLoadFor(TypeInfo(TPerson), 'Phones', false);
    Session.SetLazyLoadFor(TypeInfo(TPerson), 'Email', false);
    CheckFalse(Session.OIDIsSet(p.Car));
    CheckEquals(0, p.Phones.Count);
    CheckFalse(Session.OIDIsSet(p.Email));
    Session.LoadRelations(p, [BelongsTo]);
    CheckFalse(Session.OIDIsSet(p.Car));
    CheckEquals(0, p.Phones.Count);
    CheckFalse(Session.OIDIsSet(p.Email));
    Session.LoadRelations(p, [HasMany]);
    CheckFalse(Session.OIDIsSet(p.Car));
    CheckEquals(0, p.Phones.Count);
    CheckFalse(Session.OIDIsSet(p.Email));
    Session.LoadRelations(p, [HasOne]);
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
