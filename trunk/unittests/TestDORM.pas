unit TestDORM;

interface

uses
  TestFramework,
  dorm, BaseTestCase;

type
  TTestDORM = class(TBaseTestCase)
  protected
    procedure SetUp; override;
    function GetDORMConfigFileName: String; override;
  public
    procedure LoadPersonaPassingNilAsReturnObject;
  published
    procedure TestSave;
    procedure TestUpdate;
    procedure TestCRUD;
  end;

implementation

uses
  Classes,
  SysUtils,
  dorm.Commons, dorm.tests.bo;

{ TTestDORM }

procedure TTestDORM.TestCRUD;
var
  p1: TPerson;
  p1asstring: string;
  id: integer;
begin
  p1 := TPerson.NewPerson;
  try
    Session.Save(p1);
    p1asstring := p1.ToString;
    id := p1.id;
    Session.Commit;
  finally
    p1.Free;
  end;

  Session.StartTransaction;
  p1 := Session.Load<TPerson>(id);
  try
    CheckEquals(p1asstring, p1.ToString);
    Session.Commit;
  finally
    p1.Free;
  end;

  Session.StartTransaction;
  p1 := Session.Load<TPerson>(id);
  try
    p1.FirstName := 'Scott';
    p1.LastName := 'Summer';
    p1.Age := 45;
    p1.BornDate := EncodeDate(1965, 1, 1);
    Session.Update(p1);
    p1asstring := p1.ToString;
    Session.Commit;
  finally
    p1.Free;
  end;

  Session.StartTransaction;
  p1 := Session.Load<TPerson>(id);
  try
    CheckEquals(p1asstring, p1.ToString);
    Session.Delete(p1);
    Session.Commit;
  finally
    p1.Free;
  end;

  Session.StartTransaction;
  p1 := Session.Load<TPerson>(id);
  try
    CheckNull(p1);
    Session.Commit;
  finally
    p1.Free;
  end;
end;

function TTestDORM.GetDORMConfigFileName: String;
begin
  Result := 'dorm.conf';
end;

procedure TTestDORM.LoadPersonaPassingNilAsReturnObject;
begin
  Session.Load<TPerson>(nil);
end;

procedure TTestDORM.SetUp;
begin
  inherited;
  Session.DeleteAll(TPerson);
end;

procedure TTestDORM.TestSave;
var
  p: TPerson;
begin
  p := TPerson.NewPerson;
  try
    Session.Save(p);
    Session.Commit;
  finally
    p.Free;
  end;
  CheckEquals(1, Session.Count(TPerson));
end;

procedure TTestDORM.TestUpdate;
var
  p: TPerson;
  id: integer;
begin
  p := TPerson.Create;
  try
    p.FirstName := 'Daniele';
    p.LastName := 'Teti';
    p.Age := 30;
    p.BornDate := EncodeDate(1979, 11, 04);
    Session.Save(p);
    id := p.id;
    Session.Commit;
  finally
    p.Free;
  end;

  Session.StartTransaction;
  p := Session.Load<TPerson>(id);
  try
    p.FirstName := 'Peter';
    Session.Update(p);
    Session.Commit;
  finally
    p.Free;
  end;
  CheckEquals(1, Session.Count(TPerson));

  Session.StartTransaction;
  p := Session.Load<TPerson>(id);
  try
    CheckEquals('Peter', p.FirstName);
    Session.Commit;
  finally
    p.Free;
  end;
end;

initialization

RegisterTest(TTestDORM.Suite);

end.
