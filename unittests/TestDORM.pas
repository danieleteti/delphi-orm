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

unit TestDORM;

interface

uses
  TestFramework,
  dorm,
  dorm.Filters,
  Generics.Collections,
  dorm.Collections,
  BaseTestCase,
  DateUtils;

type
  TTestDORM = class(TBaseTestCase)
  protected
  public
    procedure SetUp; override;
    procedure LoadPersonaPassingNilAsReturnObject;
  published
    procedure TestInsert;
    procedure TestInsertWithNull;
    procedure TestUpdate;
    procedure TestCRUD;
    procedure TestCRUDAndFree;
    // procedure TestUOW;
    procedure TestDormObject;
    procedure TestDormObjectEventAfterLoad;
    procedure TestDormObjectFillListEventAfterLoad;
    procedure TestFindOne;
    procedure TestBulkCollectionOperations;

    procedure TestLoadStreamFieldAndFreeTheExistingValue;
    procedure TestStoreAndLoadUTF8StringAsBlob;
    procedure TestStoreAndLoadUTF16StringAsBlob;
  end;

implementation

uses
  Classes,
  SysUtils,
  dorm.Commons,
  dorm.tests.bo,
  dorm.UOW, dorm.Query;

{ TTestDORM }

procedure TTestDORM.TestBulkCollectionOperations;
var
  People: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
begin
  People := CreateRandomPeople;
  try
    Session.InsertCollection(People);
  finally
    People.Free
  end;

  People := {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND}.Create;
  try
    Session.LoadList<TPerson>(TdormCriteria.Create, People);
    Session.UpdateCollection(People);
  finally
    People.Free;
  end;

  People := {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND}.Create;
  try
    Session.LoadList<TPerson>(nil, People);
    Session.DeleteCollection(People);
  finally
    People.Free;
  end;
  Session.Commit;
end;

procedure TTestDORM.TestCRUD;
var
  p1: TPerson;
  p1asstring: string;
  id: integer;
begin
  p1 := TPerson.NewPerson;
  try
    Session.Insert(p1);
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
  // p1asstring: string;
  id: integer;
begin
  p1 := TPerson.NewPerson;
  id := Session.InsertAndFree(p1).AsInt64;

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
    p.Email.Value := 'not_a_real_email';
    try
      Session.Insert(p);
      Fail('There should be an exception here. The email is not valid!');
    except
      on E: Exception do
      begin
        CheckTrue(E.ClassName = 'EdormValidationException');
      end;
    end;
  finally
    p.Free;
  end;
end;

procedure TTestDORM.TestDormObjectEventAfterLoad;
var
  Email: TEmail;
  id: integer;
begin
  Email := TEmail.NewEmail;
  try
    Session.Insert(Email);
    id := Email.id;
  finally
    Email.Free;
  end;
  Email := Session.Load<TEmail>(id);
  try
    CheckEquals(UpperCase(Email.Value), Email.CopiedValue,
      'OnAfterLoad has not be called');
  finally
    Email.Free;
  end;
end;

procedure TTestDORM.TestDormObjectFillListEventAfterLoad;
var
  Email: TEmail;
  id: integer;
  emails: TObjectList<TEmail>;
begin
  Email := TEmail.NewEmail;
  try
    Session.Insert(Email);
    id := Email.id;
  finally
    Email.Free;
  end;

  emails := TObjectList<TEmail>.Create(true);
  try
    Session.LoadList<TEmail>(TdormCriteria.NewCriteria('ID', coEqual,
      id), emails);
    CheckEquals(1, emails.Count);
    CheckEquals(UpperCase(emails[0].Value), emails[0].CopiedValue,
      'OnAfterLoad has not been called');
  finally
    emails.Free;
  end;
end;

procedure TTestDORM.TestFindOne;
var
  p: TPerson;
  p1: TPerson;
begin
  p := TPerson.NewPerson;
  try
    Session.Persist(p);
    p1 := Session.Load<TPerson>(TdormCriteria.NewCriteria('ID',
      TdormCompareOperator.coEqual, p.id));
    try
      CheckEquals(p.id, p1.id); // same data
      CheckFalse(p = p1); // different objects
    finally
      p1.Free;
    end;
  finally
    p.Free;
  end;

  CheckNull(Session.Load<TPerson>(NewCriteria('ID', coLowerThan, 0)));

  Session.InsertAndFree(TPerson.NewPerson);
  Session.InsertAndFree(TPerson.NewPerson);
  ExpectedException := EdormException;
  Session.Load<TPerson>(NewCriteria('ID', coGreaterOrEqual, 1));
end;

procedure TTestDORM.LoadPersonaPassingNilAsReturnObject;
begin
  Session.Load<TPerson>(ISQLable(nil));
end;

procedure TTestDORM.SetUp;
begin
  inherited;
  Session.DeleteAll(TPerson);
  Session.DeleteAll(TCar);
  Session.DeleteAll(TEmail);
  Session.DeleteAll(TPhone);
end;

procedure TTestDORM.TestInsert;
var
  p: TPerson;
begin
  p := TPerson.Create;
  p.FirstName := 'Daniele';
  p.LastName := 'Teti';
  p.Age := 32;
  p.BornDate := EncodeDate(1979, 11, 4);
  p.BornTimeStamp := EncodeDateTime(1979, 11, 4, 16, 10, 00, 0);
  try
    Session.Insert(p);
    Session.Commit;
  finally
    p.Free;
  end;
  CheckEquals(1, Session.Count(TPerson));
end;

procedure TTestDORM.TestInsertWithNull;
var
  p: TPerson;
  id: integer;
begin
  p := TPerson.Create;
  p.FirstName := 'Daniele';
  p.LastName := '';
  p.Age := 0;
  p.BornDate := 0;
  p.BornTimeStamp := EncodeDateTime(1979, 11, 4, 16, 10, 00, 0);
  try
    Session.Insert(p);
    id := p.id;
    Session.Commit;
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(id);
  try
    CheckEquals(0, p.BornDate); // this field should be null on database
    CheckEquals(0, p.Age); // this field should be null on database
  finally
    p.Free;
  end;
end;

procedure TTestDORM.TestLoadStreamFieldAndFreeTheExistingValue;
var
  p: TPerson;
  id: integer;
begin
  p := TPerson.NewPerson;
  p.Photo := TStringStream.Create('photo');
  try
    Session.Insert(p);
    id := p.id;
    Session.Commit;
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(id);
  try
    // this field should be a memory stream,
    // this means that dormUtils has freed the field
    // and recrteated it like a MemoryStream (Photo is a TStream)
    CheckTrue(p.Photo is TMemoryStream);
  finally
    p.Free;
  end;
end;

procedure TTestDORM.TestStoreAndLoadUTF16StringAsBlob;
var
  p: TPerson;
  id: integer;
  ss: TStringStream;
  str: String;
begin
  // ARRANGE
  str := 'This is a unicode text encoded as UTF16 [Delphi Default] (什么是) end of text';
  // ACT
  p := TPerson.NewPerson;

  p.Photo := TStringStream.Create(str, TEncoding.Unicode);

  { *** some others way to do the same thing *** --daniele^2 }
  // this is if you want to encode the stream in UTF8
  // p.Photo := TStringStream.Create('', TEncoding.UTF8);

  // this is the same using UTF8Encode function
  // p.Photo := TStringStream.Create(UTF8Encode(str), TEncoding.UTF8);
  try
    Session.Insert(p);
    id := p.id;
    Session.Commit;
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(id);
  try
    // ASSERT
    ss := TStringStream.Create('',
      TEncoding.Unicode { use the same encoding used while writing } );
    try
      ss.LoadFromStream(p.Photo);
      CheckEquals(str, ss.DataString);
    finally
      ss.Free;
    end;

  finally
    p.Free;
  end;
end;

procedure TTestDORM.TestStoreAndLoadUTF8StringAsBlob;
var
  p: TPerson;
  id: integer;
  ss: TStringStream;
  str: UTF8String;
begin
  // ARRANGE
  str := 'This is a unicode text encoded as UTF8 (什么是) end of text';
  // ACT
  p := TPerson.NewPerson;
  p.Photo := TStringStream.Create(str, TEncoding.UTF8);
  try
    Session.Insert(p);
    id := p.id;
    Session.Commit;
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(id);
  try
    // ASSERT
    ss := TStringStream.Create('', TEncoding.UTF8);
    try
      ss.LoadFromStream(p.Photo);
      CheckEquals(str, ss.DataString);
    finally
      ss.Free;
    end;

  finally
    p.Free;
  end;

end;

// procedure TTestDORM.TestUOW;
// var
// UOW: TdormUOW;
// p1: TPerson;
// p2: TPerson;
// begin
// p1 := TPerson.NewPerson;
// try
// p1.FirstName := 'John';
// p1.LastName := 'Doe';
// p2 := TPerson.NewPerson;
// try
// UOW := TdormUOW.Create;
// try
// p1.FirstName := 'Scott';
// p1.LastName := 'Summer';
// CheckFalse(Session.OIDIsSet(p1));
// UOW.AddInsert(p1);
// Session.Save(UOW);
//
// UOW.FreeDeleted; // free nothing... there aren't "deleted" objects
// CheckTrue(Session.OIDIsSet(p1));
// UOW.Clear;
//
// p1.FirstName := 'John';
// UOW.AddUpdate(p1);
// Session.Save(UOW);
// CheckTrue(Session.OIDIsSet(p1));
// UOW.Clear;
// UOW.AddDelete(p1);
// Session.Save(UOW);
// UOW.GetUOWDelete.Extract(p1);
// UOW.AddInsert(p1);
// // This will not actually add the object to the collection.
// UOW.AddInsert(p1);
// CheckEquals(1, UOW.GetUOWInsert.Count);
// Session.Save(UOW);
// CheckTrue(Session.OIDIsSet(p1));
// finally
// UOW.Free;
// end;
//
// UOW := TdormUOW.Create;
// try
// Session.ClearOID(p1);
// Session.ClearOID(p1.Car); // contained objects
// Session.ClearOID(p1.Email); // contained objects
// Session.ClearOID(p2);
// UOW.AddInsert(p1);
// UOW.AddDelete(p2);
// Session.Save(UOW);
// finally
// UOW.Free;
// end;
// finally
// p2.Free;
// end;
// finally
// p1.Free;
// end;
//
// end;

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
    Session.Insert(p);
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
