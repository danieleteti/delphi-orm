unit TestDORMRelations;

interface

uses
  TestFramework,
  TypInfo,
  dorm,
  dorm.Commons, BaseTestCase;

type
  TTestDORMHasMany = class(TBaseTestCase)
  protected
    function GetDORMConfigFileName: String; override;
  public
    procedure Setup; override;
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

{ TTestDORMHasMany }

procedure TTestDORMHasMany.Setup;
begin
  inherited;
end;

procedure TTestDORMHasMany.TearDown;
begin
  inherited;
end;

function TTestDORMHasMany.GetDORMConfigFileName: String;
begin
  Result := 'dorm.conf';
end;

procedure TTestDORMHasMany.TestHasManyLazyLoad;
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
  Session.SetLazyLoadFor(TypeInfo(TPerson), 'Telefoni', true);
  p := Session.Load<TPerson>(id);
  try
    CheckFalse(Assigned(p.Phones));
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  // Test with lazy load OFF
  Session.SetLazyLoadFor(TypeInfo(TPerson), 'Telefoni', false);
  p := Session.Load<TPerson>(id);
  // Without commit, AV becouse IdentityMap doesn't work propely
  try
    CheckEquals(1, p.Phones.Count); // Child objects are loaded
    Session.Commit;
  finally
    p.Free;
  end;
end;

procedure TTestDORMHasMany.TestLoadHasMany;
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
    Session.Commit;
  finally
    p.Free;
  end;
end;

initialization

RegisterTest(TTestDORMHasMany.Suite);

finalization

end.
