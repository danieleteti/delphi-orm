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
    procedure TestCRUDAndFree;
    procedure TestUOW;
    procedure TestDormObject;
  end;

implementation

uses
  Classes,
  SysUtils,
  dorm.Commons, dorm.tests.bo, dorm.UOW;

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

procedure TTestDORM.TestCRUDAndFree;
var
  p1: TPerson;
  p1asstring: string;
  id: integer;
begin
  p1 := TPerson.NewPerson;
  id := Session.SaveAndFree(p1).AsInt64;

  p1 := Session.Load<TPerson>(id);
  try
    CheckEquals(id, p1.id);
  finally
    p1.Free;
  end;

  p1 := Session.Load<TPerson>(id);
  p1.FirstName := 'Scott';
  p1.LastName := 'Summer';
  p1.Age := 45;
  p1.BornDate := EncodeDate(1965, 1, 1);
  Session.UpdateAndFree(p1);

  p1 := Session.Load<TPerson>(id);
  Session.DeleteAndFree(p1);

  p1 := Session.Load<TPerson>(id);
  try
    CheckNull(p1);
  finally
    p1.Free;
  end;
end;

procedure TTestDORM.TestDormObject;
var
  p: TPerson;
begin
  p := TPerson.NewPerson;
  try
    p.Email := TEmail.Create;
    p.Email.Value := 'not_a_real_email';
    try
      Session.Save(p);
      Fail('There should be an exception here. The email is not valid!');
    except
      on E: Exception do
      begin
        CheckTrue(E.ClassName = 'EdormException');
      end;
    end;
  finally
    p.Free;
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

procedure TTestDORM.TestUOW;
var
  UOW: TdormUOW;
  p1: TPerson;
  p2: TPerson;
begin
  UOW := TdormUOW.Create;
  try
    p1 := TPerson.NewPerson;
    try
      p1.FirstName := 'John';
      p1.LastName := 'Doe';
      p2 := TPerson.NewPerson;
      try
        p1.FirstName := 'Scott';
        p1.LastName := 'Summer';
        CheckFalse(Session.OIDIsSet(p1));
        UOW.AddInsert(p1);
        Session.Save(UOW);
        CheckTrue(Session.OIDIsSet(p1));
        UOW.Clear;
        p1.FirstName := 'John';
        UOW.AddUpdate(p1);
        Session.Save(UOW);
        CheckTrue(Session.OIDIsSet(p1));
        UOW.Clear;
        UOW.AddDelete(p1);
        Session.Save(UOW);
        UOW.GetUOWDelete.Extract(p1);
        // otherwise, p1 is owned by the "delete" TdormCollection
        Session.ClearOID(p1);
        UOW.AddInsert(p1);
        // This will not actually add the object to the collection.
        UOW.AddInsert(p1);
        CheckEquals(1, UOW.GetUOWInsert.Count);
        Session.Save(UOW);
        CheckTrue(Session.OIDIsSet(p1));
      finally
        p2.Free;
      end;
    finally
      p1.Free;
    end;
  finally
    UOW.Free;
  end;
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
