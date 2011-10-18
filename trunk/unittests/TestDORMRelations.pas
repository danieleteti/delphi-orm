unit TestDORMRelations;

interface

uses
  TestFramework,
  TypInfo,
  dorm,
  dorm.Commons, BaseTestCase;

type
  TTestDORMRelations = class(TBaseTestCase)
  protected
    function GetDORMConfigFileName: String; override;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoadHasMany;
    procedure TestHasManyLazyLoad;
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

function TTestDORMRelations.GetDORMConfigFileName: String;
begin
  Result := 'dorm.conf';
end;

procedure TTestDORMRelations.TestHasManyLazyLoad;
var
  p: TPerson;
  t: TPhone;
  id: Integer;
begin
  p := TPerson.NewPerson;
  try
    p.Phones := NewList;
    t := TPhone.Create;
    p.Phones.Add(t);
    Session.Save(p);
    id := p.id;
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  // Now Test with lazy load ON
  Session.SetLazyLoadFor(TypeInfo(TPerson), 'Phones', true);
  p := Session.Load<TPerson>(id);
  try
    CheckFalse(Assigned(p.Phones));
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  // Test with lazy load OFF
  Session.SetLazyLoadFor(TypeInfo(TPerson), 'Phones', false);
  p := Session.Load<TPerson>(id);
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
  id: Integer;
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
    id := p.id;
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  p := Session.Load<TPerson>(id);
  try
    CheckEquals(2, p.Phones.Count);
    CheckEquals('Ford', p.Car.Brand);
    CheckEquals('Focus 1.8 TDCi', p.Car.Model);
    Session.Commit;
  finally
    p.Free;
  end;
end;

initialization

RegisterTest(TTestDORMRelations.Suite);

finalization

end.
