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

unit TestDORM;

interface

uses
  TestFramework,
  dorm, BaseTestCase;

type
  TTestDORM = class(TBaseTestCase)
  protected
    procedure SetUp; override;
  public
    procedure LoadPersonaPassingNilAsReturnObject;
  published
    procedure TestSave;
    procedure TestUpdate;
    procedure TestCRUD;
    procedure TestCRUDAndFree;
    procedure TestUOW;
    procedure TestDormObject;
    procedure TestFindOne;
    procedure TestFindOneNoObject;
    procedure TestFindOneManyObjects;
  end;

implementation

uses
  Classes,
  SysUtils,
  Rtti,
  dorm.Commons, dorm.tests.bo, dorm.UOW;

function FirstNameIs(const FirstName: String): TdormCriteria;
begin
  Result := TdormCriteria.Create;
  Result.Add('FirstName', Equal, TValue.From(FirstName));
end;

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

procedure TTestDORM.TestFindOne;
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

  p := Session.FindOne(TypeInfo(TPerson), FirstNameIs('Daniele')) as TPerson;
  try
    CheckNotNull(p, 'Didn''t find the object');
  finally
    p.Free;
  end;

  p := Session.FindOne<TPerson>(FirstNameIs('Daniele'));
  try
    CheckNotNull(p, 'Didn''t find the object');
  finally
    p.Free;
  end;
end;

procedure TTestDORM.TestFindOneManyObjects;
var
  p: TPerson;
begin
  p := TPerson.NewPerson;
  try
    Session.Save(p);
  finally
    p.Free;
  end;

  p := TPerson.NewPerson;
  try
    Session.Save(p);
  finally
    p.Free;
  end;

  Session.Commit;

  try
    Session.FindOne(TypeInfo(TPerson), FirstNameIs('Daniele'));
    Fail('TSession.FindOne must raise an EdormException when more than one object is found');
  except
    on EdormException do CheckTrue(True);
  end;

  try
    Session.FindOne<TPerson>(FirstNameIs('Daniele'));
    Fail('TSession.FindOne must raise an EdormException when more than one object is found');
  except
    on EdormException do CheckTrue(True);
  end;
end;

procedure TTestDORM.TestFindOneNoObject;
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

  p := Session.FindOne(TypeInfo(TPerson), FirstNameIs('SomeOneThatDoesntExist')) as TPerson;
  try
    CheckNull(p, 'TSession.FindOne should return nil then no object is found');
  finally
    p.Free;
  end;

  p := Session.FindOne<TPerson>(FirstNameIs('SomeOneThatDoesntExist'));
  try
    CheckNull(p, 'TSession.FindOne should return nil then no object is found');
  finally
    p.Free;
  end;
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
