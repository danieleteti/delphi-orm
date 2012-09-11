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

unit FrameworkTests;

interface

uses
  TestFramework,
  BaseTestCase,
  Generics.Collections,
  dorm.tests.bo,
  dorm,
  dorm.Filters,
  dorm.Collections,
  dorm.Commons,
  dorm.InterposedObject,
  Generics.Defaults;

type
{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished]) METHODS([vcPrivate, vcProtected, vcPublic, vcPublished]) PROPERTIES([vcPrivate, vcProtected, vcPublic, vcPublished])}
  TPeople = class({$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND})
  protected
    function GetElement(const index: Integer): TPerson;
  end;

  TFrameworkTests = class(TBaseTestCase)
  published
    procedure TestGeneratorID;
    procedure TestLoadByAttribute;
    procedure TestLoadByAttributeWithRelationOperators;
    procedure TestClone;
    procedure TestCloneWithStream;
    procedure TestCopyObjectWithStream;
    procedure TestTDate;
    procedure TestTDateTime;
    procedure TestBlob;
    procedure TestEnumerableCollection;
    procedure TestLastInsertedOID;
    procedure TestCollectionSorting;
    procedure TestListDuckTyping;
    procedure TestWrapAsList;
  end;

  TObjectStatusModelTests = class(TBaseTestCase)
  published
    procedure TestDirtyLifeCycle;
  end;

  TPersonComparer = class(TComparer<TObject>)
    function Compare(const Left, Right: TObject): Integer; override;
  end;

implementation

uses
  Classes,
  SysUtils,
  DateUtils,
  dorm.Utils,
  IdHashMessageDigest,
  idHash,
  RTTI;

// returns MD5 has for a file
function MD5(const fileName: string): string;
var
  idmd5: TIdHashMessageDigest5;
  fs: TFileStream;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    fs := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
    try
      result := idmd5.HashStreamAsHex(fs);
    finally
      fs.Free;
    end;
  finally
    idmd5.Free;
  end;
end;

{ TFrameworkTests }

procedure TFrameworkTests.TestBlob;
var
  p: TPerson;
  fs: TFileStream;
  oid: Integer;
  md5before, md5after: string;
begin
  p := TPerson.NewPerson;
  try
    p.Photo := TMemoryStream.Create;
    md5before := MD5('photo.png');
    fs := TFileStream.Create('photo.png', fmOpenRead or fmShareDenyWrite);
    try
      p.Photo.CopyFrom(fs, 0);
    finally
      fs.Free;
    end;
    Session.Insert(p);
    Session.Commit;
    oid := p.ID;
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(oid);
  try
    if FileExists('reloaded_photo.png') then
      DeleteFile('reloaded_photo.png');
    fs := TFileStream.Create('reloaded_photo.png', fmCreate);
    try
      fs.CopyFrom(p.Photo, 0);
    finally
      fs.Free;
    end;
  finally
    p.Free;
  end;
  md5after := MD5('reloaded_photo.png');
  CheckEquals(md5before, md5after);

  p := Session.Load<TPerson>(oid);
  try
    p.Photo.Free;
    p.Photo := nil;
    Session.Update(p);
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(oid);
  try
    CheckNull(p.Photo);
  finally
    p.Free;
  end;
end;

procedure TFrameworkTests.TestClone;
var
  p1, p2: TPerson;
begin
  p1 := TPerson.NewPerson;
  p2 := Session.Clone<TPerson>(p1);
  CheckEquals(p1.FirstName, p2.FirstName);
  CheckEquals(p1.Age, p2.Age);
  CheckEquals(p1.ID, p2.ID);
  p1.Free;
  p2.Free;
end;

procedure TFrameworkTests.TestCloneWithStream;
var
  p: TPerson;
  fs: TFileStream;
  cloned_p: TPerson;
begin
  p := TPerson.NewPerson;
  try
    p.Photo := TMemoryStream.Create;
    p.Phones.Add(TPhone.Create);
    p.Phones.Add(TPhone.Create);
    p.Phones.Add(TPhone.Create);
    fs := TFileStream.Create('photo.png', fmOpenRead or fmShareDenyWrite);
    try
      p.Photo.CopyFrom(fs, 0);
    finally
      fs.Free;
    end;
    cloned_p := Session.Clone<TPerson>(p);
    try
      CheckEquals(p.ID, cloned_p.ID);
      CheckEquals(p.FirstName, cloned_p.FirstName);
      CheckEquals(p.LastName, cloned_p.LastName);
      CheckEquals(p.Age, cloned_p.Age);
      CheckEquals(p.BornDate, cloned_p.BornDate);
      CheckEquals(p.BornTimeStamp, cloned_p.BornTimeStamp);
      CheckEquals(p.Phones.Count, cloned_p.Phones.Count);
      CheckEquals(p.Car.Brand, cloned_p.Car.Brand);
      CheckEquals(p.Car.Model, cloned_p.Car.Model);
      CheckEquals(p.Email.Value, cloned_p.Email.Value);
      CheckEquals(p.Photo.Size, cloned_p.Photo.Size);
    finally
      cloned_p.Free;
    end;
  finally
    p.Free;
  end;

end;

procedure TFrameworkTests.TestCollectionSorting;
var
  Coll: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  p: TPerson;
  cmp: TdormComparer<TPerson>;
  revCmp: TdormReverseComparer<TPerson>;
begin
  Coll := {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND}.
    Create(true);
  try
    p := TPerson.NewPerson;
    p.LastName := 'abc';
    p.Age := 30;
    p.BornDate := EncodeDate(2000, 1, 10);
    Coll.Add(p);
    p := TPerson.NewPerson;
    p.LastName := 'bcd';
    p.Age := 29;
    p.BornDate := EncodeDate(2000, 2, 10);
    Coll.Add(p);
    p := TPerson.NewPerson;
    p.LastName := 'cde';
    p.Age := 28;
    p.BornDate := EncodeDate(2000, 3, 10);
    Coll.Add(p);
    p := TPerson.NewPerson;
    p.LastName := 'def';
    p.Age := 27;
    p.BornDate := EncodeDate(2000, 4, 10);
    Coll.Add(p);

    cmp := TdormComparer<TPerson>.Create('LastName');
    Coll.Sort(cmp);
    cmp.Free;
    CheckEquals('abc', TPerson(Coll[0]).LastName);

    revCmp := TdormReverseComparer<TPerson>.Create('LastName');
    Coll.Sort(revCmp);
    revCmp.Free;
    CheckEquals('def', TPerson(Coll[0]).LastName);

    cmp := TdormComparer<TPerson>.Create('Age');
    Coll.Sort(cmp);
    cmp.Free;
    CheckEquals(27, TPerson(Coll[0]).Age);

    revCmp := TdormReverseComparer<TPerson>.Create('Age');
    Coll.Sort(revCmp);
    revCmp.Free;
    CheckEquals(30, TPerson(Coll[0]).Age);

    cmp := TdormComparer<TPerson>.Create('BornDate');
    Coll.Sort(cmp);
    cmp.Free;

    CheckEquals(EncodeDate(2000, 1, 10), TPerson(Coll[0]).BornDate);
    revCmp := TdormReverseComparer<TPerson>.Create('BornDate');
    Coll.Sort(revCmp);
    revCmp.Free;
    CheckEquals(EncodeDate(2000, 4, 10), TPerson(Coll[0]).BornDate);

  finally
    Coll.Free;
  end;
end;

procedure TFrameworkTests.TestCopyObjectWithStream;
var
  p: TPerson;
  fs: TFileStream;
  cloned_p: TPerson;
begin
  p := TPerson.NewPerson;
  try
    p.Photo := TMemoryStream.Create;
    fs := TFileStream.Create('photo.png', fmOpenRead or fmShareDenyWrite);
    try
      p.Photo.CopyFrom(fs, 0);
    finally
      fs.Free;
    end;
    cloned_p := TPerson.Create;
    Session.CopyObject(p, cloned_p);
    try
      CheckEquals(p.ID, cloned_p.ID);
      CheckEquals(p.FirstName, cloned_p.FirstName);
      CheckEquals(p.LastName, cloned_p.LastName);
      CheckEquals(p.Age, cloned_p.Age);
      CheckEquals(p.BornDate, cloned_p.BornDate);
      CheckEquals(p.BornTimeStamp, cloned_p.BornTimeStamp);
      CheckEquals(0, cloned_p.Phones.Count);
      CheckFalse(Session.OIDIsSet(cloned_p.Car));
      CheckEquals(p.Photo.Size, cloned_p.Photo.Size);
    finally
      cloned_p.Free;
    end;
  finally
    p.Free;
  end;
end;

{$HINTS OFF}


procedure TFrameworkTests.TestEnumerableCollection;
var
  people: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  person: TObject;
  I: Integer;
  x: Integer;
  p: TPerson;
begin
  Session.DeleteAll(TPerson);
  people := Session.LoadList<TPerson>;
  try
    for person in people do
      Fail('There arent records!!!');
  finally
    people.Free;
  end;

  for I := 1 to 10 do
  begin
    p := TPerson.NewPerson;
    p.FirstName := p.FirstName + inttostr(I);
    Session.Insert(p);
    p.Free;
  end;

  people := Session.LoadList<TPerson>;
  try
    x := 0;
    for I := 0 to (people.Count - 1) do
      inc(x);
    CheckEquals(10, x);
  finally
    people.Free;
  end;
end;
{$HINTS ON}


procedure TFrameworkTests.TestGeneratorID;
var
  T: TPerson;
begin
  T := TPerson.NewPerson;
  try
    CheckTrue(T.ID = 0);
    Session.Insert(T);
    CheckFalse(T.ID = 0);
  finally
    T.Free;
  end;
end;

procedure TFrameworkTests.TestLastInsertedOID;
var
  p: TPerson;
begin
  CheckTrue(Session.Strategy.GetLastInsertOID.IsEmpty);
  p := TPerson.NewPerson;
  try
    // If Person has Email and Car, the LastInsertedOID will be the Email or Car.
    // But I need to test the LastInsertedOID using a Person. So I remove the
    // related objects.

    p.Email.Free;
    p.Email := nil;
    p.Car.Free;
    p.Car := nil;
    Session.Insert(p);
    CheckTrue(p.ID = Session.Strategy.GetLastInsertOID.AsInt64);
  finally
    p.Free;
  end;
end;

{$HINTS OFF}


procedure TFrameworkTests.TestListDuckTyping;
var
{$IF CompilerVersion = 22}
  ObjectList: TPeople;
{$ELSE}
  ObjectList: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
{$IFEND}
  List: IWrappedList;
  o: TObject;
  p: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  c: Integer;
begin
  ObjectList :=
{$IF CompilerVersion = 22}
    TPeople.Create;
{$ELSE}
{$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND}.Create;
{$IFEND}
  try
    List := WrapAsList(ObjectList);
    for o in List do
      Fail('There arent records, so you should not enter in the loop');
    p := CreateRandomPeople;
    try
      c := p.Count;
      Session.DeleteAll(TPerson);
      Session.InsertCollection(p);
      p.Clear;
      Session.FillList<TPerson>(p);
      CheckEquals(c, p.Count);
      c := 0;
      List := WrapAsList(p);
      for o in List do
        inc(c);
      CheckEquals(c, p.Count);
    finally
      p.Free;
    end;
  finally
    ObjectList.Free;
  end;
end;
{$HINTS ON}


procedure TFrameworkTests.TestLoadByAttribute;
var
  Coll: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  Criteria: ICriteria;
  m_id: Int64;
  p: TPerson;
  obj: TPerson;
begin
  m_id := 0;
  Session.DeleteAll(TPerson);

  p := TPerson.NewPerson;
  try
    p.Age := 32;
    Session.Insert(p);
  finally
    p.Free;
  end;

  p := TPerson.NewPerson;
  try
    p.Age := 30;
    p.FirstName := 'Jack';
    Session.Insert(p);
  finally
    p.Free;
  end;

  Criteria := TdormCriteria.Create;

  Criteria._And('FirstName', coEqual, 'Daniele')._And('FirstName',
    coNotEqual, 'Jack');
  Coll := Session.LoadList<TPerson>(Criteria);
  try
    CheckEquals(1, Coll.Count);
    for obj in Coll do
      m_id := obj.ID;
  finally
    Coll.Free;
  end;

  Criteria._And('ID', coEqual, m_id);
  Coll := Session.LoadList<TPerson>(Criteria);
  try
    CheckEquals(1, Coll.Count);
  finally
    Coll.Free;
  end;

  Criteria._And('ID', coGreaterThan, m_id);
  Coll := Session.LoadList<TPerson>(Criteria);
  try
    CheckEquals(0, Coll.Count);
  finally
    Coll.Free;
  end;
end;

procedure TFrameworkTests.TestLoadByAttributeWithRelationOperators;
var
  Coll: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  Criteria: ICriteria;
  p: TPerson;
begin
  Session.DeleteAll(TPerson);

  p := TPerson.NewPerson;
  try
    p.Age := 32;
    Session.Insert(p);
  finally
    p.Free;
  end;

  p := TPerson.NewPerson;
  try
    p.Age := 30;
    Session.Insert(p);
  finally
    p.Free;
  end;

  Criteria := TdormCriteria.NewCriteria('FirstName', coEqual, 'Daniele')
    ._Or('LastName', coEqual, 'Teti');
  Coll := Session.LoadList<TPerson>(Criteria);
  try
    CheckEquals(2, Coll.Count);
  finally
    Coll.Free;
  end;

  Criteria := TdormCriteria.NewCriteria('Age', coEqual, 32);
  Coll := Session.LoadList<TPerson>(Criteria);
  try
    CheckEquals(1, Coll.Count);
  finally
    Coll.Free;
  end;
end;

procedure TFrameworkTests.TestTDate;
var
  p: TPerson;
  dt: TDateTime;
  p_id: Int64;
begin
  dt := EncodeDate(2011, 10, 30);

  p := TPerson.NewPerson;
  try
    p.BornDate := dt;
    Session.Insert(p);
    p_id := p.ID;
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(p_id);
  try
    CheckEquals(dt, p.BornDate);
    Session.Delete(p);
  finally
    p.Free;
  end;
end;

procedure TFrameworkTests.TestTDateTime;
var
  p: TPerson;
  dt: TDateTime;
  p_id: Int64;
begin
  dt := EncodeDateTime(2011, 10, 30, 10, 20, 30, 0);
  // do not support milliseconds...

  p := TPerson.NewPerson;
  try
    p.BornTimeStamp := dt;
    Session.Insert(p);
    p_id := p.ID;
  finally
    p.Free;
  end;

  p := Session.Load<TPerson>(p_id);
  try
    CheckEquals(dt, p.BornTimeStamp);
    Session.Delete(p);
  finally
    p.Free;
  end;

end;

procedure TFrameworkTests.TestWrapAsList;
var
  List: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  WrappedList: IWrappedList;
  obj: TObject;
  c: Integer;
begin
  List := {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND}.
    Create(true);
  try
    List.Add(TPerson.NewPerson);
    List.Add(TPerson.NewPerson);
    List.Add(TPerson.NewPerson);
    List.Add(TPerson.NewPerson);
    c := 0;
    WrappedList := WrapAsList(List);
    for obj in WrappedList do
    begin
      inc(c);
      CheckEquals('Daniele', TPerson(obj).FirstName);
    end;
    CheckEquals(4, c);
  finally
    List.Free;
  end;
end;

{ TPersonComparer }

function TPersonComparer.Compare(const Left, Right: TObject): Integer;
var
  L: TPerson;
  R: TPerson;
begin
  L := TPerson(Left);
  R := TPerson(Right);
  if L.LastName > R.LastName then
    Exit(1);
  if L.LastName < R.LastName then
    Exit(-1);
  result := 0;
end;

{ TPeople }

function TPeople.GetElement(const index: Integer): TPerson;
begin
  result := Items[index];
end;

{ TObjectStatusModelTests }

{ todo: this test is really really too big. I've to refactor in a million of other test methods }
procedure TObjectStatusModelTests.TestDirtyLifeCycle;
var
  p: TPersonDirty;
  ID: Integer;
  p1: TPerson;
  phone1: TPhoneDirty;
  Phone1ID: Integer;
  Phone2ID: Integer;
  Car: TCar;
  p2: TPersonDirty;
begin
  p := TPersonDirty.Create;
  try
    CheckTrue(p.ObjStatus = osDirty);
    Session.SetObjectStatus(p, osClean);
    CheckTrue(p.ObjStatus = osClean);
    Session.SetObjectStatus(p, osUnknown);
    CheckTrue(p.ObjStatus = osUnknown);
    Session.SetObjectStatus(p, osDeleted);
    CheckTrue(p.ObjStatus = osDeleted);
  finally
    p.Free;
  end;

  p := TPersonDirty.Create;
  try
    p.Car.Free;
    p.Car := TCarDirty.Create; // I'm too lazy to creanote another TPhone with a differetn TCar type

    p.Email.Free;
    p.Email := TEmailDirty.Create;
    p.Email.Value := 'daniele.teti@gmail.com';

    p.FirstName := 'Daniele';
    p.LastName := 'Teti';
    p.Age := 32;
    p.BornDate := EncodeDate(1979, 11, 4);
    p.BornTimeStamp := EncodeDateTime(1979, 11, 4, 16, 10, 00, 0);
    phone1 := TPhoneDirty.Create;
    phone1.Number := '324-678654';
    phone1.Model := 'Samsung Galaxy S';
    p.Phones.Add(phone1);
    phone1 := TPhoneDirty.Create;
    phone1.Number := '876-12121212';
    phone1.Model := 'Samsung Galaxy S III';
    p.Phones.Add(phone1);
    Session.Persist(p); // insert object and his relations
    ID := p.ID;
    Phone1ID := TPhoneDirty(p.Phones.Items[0]).ID;
    Phone2ID := TPhoneDirty(p.Phones.Items[1]).ID;

    CheckTrue(p.ObjStatus = osClean);
    CheckTrue(TPhoneDirty(p.Phones.Items[0]).ObjStatus = osClean);
    CheckTrue(TPhoneDirty(p.Phones.Items[1]).ObjStatus = osClean);

    phone1 := Session.Load<TPhoneDirty>(Phone1ID);
    CheckNotNull(phone1);
    phone1.Free;

    phone1 := Session.Load<TPhoneDirty>(Phone2ID);
    CheckNotNull(phone1);
    phone1.Free;

    Session.Persist(p); // do nothing
    CheckTrue(p.ObjStatus = osClean);
    CheckTrue(TPhoneDirty(p.Phones.Items[0]).ObjStatus = osClean);
    CheckTrue(TPhoneDirty(p.Phones.Items[1]).ObjStatus = osClean);
    CheckEquals(1, Session.Count(TPerson));

    TPhoneDirty(p.Phones.Items[0]).Model := 'changed model';
    TPhoneDirty(p.Phones.Items[0]).ObjStatus := osDirty;
    Session.Persist(p); // do nothing
    phone1 := Session.Load<TPhoneDirty>(Phone1ID);
    CheckNotNull(phone1);
    CheckEquals('changed model', phone1.Model);
    phone1.Free;

    // check the has one relation
    p.Car.Brand := 'Ford';
    p.Car.Model := 'Focus';
    Session.Persist(p);

    Car := Session.Load<TCarDirty>(p.Car.ID);
    CheckNotNull(Car);
    Car.Free;

    // One last check
    Session.Commit;
    Session.StartTransaction;
    p2 := Session.Load<TPersonDirty>(p.ID);
    try
      CheckEquals(2, p2.Phones.Count);
      CheckEquals('Ford', p2.Car.Brand);
      CheckEquals('Focus', p2.Car.Model);
      CheckEquals('daniele.teti@gmail.com', p2.Email.Value);
    finally
      p2.Free;
    end;

    p.ObjStatus := osDeleted;
    Session.Persist(p); // delete person and related objects
    p1 := Session.Load<TPerson>(ID);
    CheckNull(p1, 'Person is not deleted');
    Session.Commit;
    CheckEquals(0, Session.Count(TPerson));
  finally
    p.Free;
  end;
end;

initialization

RegisterTest(TFrameworkTests.Suite);
RegisterTest(TObjectStatusModelTests.Suite);

end.
