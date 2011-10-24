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

unit FrameworkTests;

interface

uses
  TestFramework,
  BaseTestCase,
  dorm,
  dorm.Collections,
  dorm.Commons;

type
  TFrameworkTests = class(TBaseTestCase)
  published
    procedure TestFirebirdGeneratorID;
    procedure TestLoadByAttribute;
    procedure TestLoadByAttributeWithRelationOperators;
    procedure TestClone;
    procedure TestCloneWithStream;
    procedure TestCopyObjectWithStream;
    procedure TestTDate;
    procedure TestTDateTime;
    procedure TestBlob;
    procedure TestEnumerableCollection;
  end;

implementation

uses
  Classes,
  SysUtils,
  DateUtils,
  dorm.Utils, dorm.tests.bo,
  IdHashMessageDigest,
  idHash;

// returns MD5 has for a file
function MD5(const fileName: string): string;
var
  idmd5: TIdHashMessageDigest5;
  fs: TFileStream;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    fs := TFileStream.Create(fileName, fmOpenRead OR fmShareDenyWrite);
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
    Session.Save(p);
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
    p.Car := TCar.NewCar;
    p.Email := TEmail.NewEmail;
    p.Photo := TMemoryStream.Create;
    p.Phones := NewList();
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
      CheckNull(cloned_p.Phones);
      CheckNull(cloned_p.Car);
      CheckEquals(p.Photo.Size, cloned_p.Photo.Size);
    finally
      cloned_p.Free;
    end;
  finally
    p.Free;
  end;
end;

procedure TFrameworkTests.TestEnumerableCollection;
var
  people: TdormCollection;
  person: TObject;
  I: Integer;
  x: Integer;
  p: TPerson;
begin
  Session.DeleteAll(TPerson);
  people := Session.ListAll<TPerson>;
  try
    for person in people do
      Fail('There arent record!!!');
  finally
    people.Free;
  end;

  for I := 1 to 10 do
  begin
    p := TPerson.NewPerson;
    p.FirstName := p.FirstName + inttostr(I);
    Session.Save(p);
    p.Free;
  end;

  people := Session.ListAll<TPerson>;
  try
    x := 0;
    for person in people do
      inc(x);
    CheckEquals(10, x);
  finally
    people.Free;
  end;
end;

procedure TFrameworkTests.TestFirebirdGeneratorID;
// var
// t: TTest01;
begin
  // t := TTest01.Create;
  // try
  // CheckTrue(t.ID = 0);
  // t.RagioneSociale := 'Daniele Teti Corp';
  // Session.Save(t);
  // CheckFalse(t.ID = 0);
  // finally
  // t.Free;
  // end;
end;

procedure TFrameworkTests.TestLoadByAttribute;
var
  coll: TdormCollection;
  Criteria: TdormCriteria;
  m_id: Int64;
  p: TPerson;
begin
  Session.DeleteAll(TPerson);

  p := TPerson.NewPerson;
  try
    p.Age := 32;
    Session.Save(p);
  finally
    p.Free;
  end;

  p := TPerson.NewPerson;
  try
    p.Age := 30;
    p.FirstName := 'Jack';
    Session.Save(p);
  finally
    p.Free;
  end;

  Criteria := TdormCriteria.Create;
  try
    Criteria.Add('FirstName', Equal, 'Daniele').AddAnd('FirstName',
      Different, 'Jack');
    coll := Session.List<TPerson>(Criteria, false);
    try
      CheckEquals(1, coll.Count);
      m_id := TPerson(coll.GetItem(0)).ID;
    finally
      coll.Free;
    end;

    Criteria.Add('ID', Equal, m_id);
    coll := Session.List<TPerson>(Criteria, false);
    try
      CheckEquals(1, coll.Count);
    finally
      coll.Free;
    end;

    Criteria.Add('ID', GreaterThan, m_id);
    coll := Session.List<TPerson>(Criteria, false);
    try
      CheckEquals(0, coll.Count);
    finally
      coll.Free;
    end;
  finally
    Criteria.Free;
  end;
end;

procedure TFrameworkTests.TestLoadByAttributeWithRelationOperators;
var
  coll: TdormCollection;
  Criteria: TdormCriteria;
  p: TPerson;
begin
  Session.DeleteAll(TPerson);

  p := TPerson.NewPerson;
  try
    p.Age := 32;
    Session.Save(p);
  finally
    p.Free;
  end;

  p := TPerson.NewPerson;
  try
    p.Age := 30;
    Session.Save(p);
  finally
    p.Free;
  end;

  Criteria := TdormCriteria.NewCriteria('FirstName', Equal, 'Daniele')
    .AddOr('LastName', Equal, 'Teti');
  coll := Session.List<TPerson>(Criteria, true);
  try
    CheckEquals(2, coll.Count);
  finally
    coll.Free;
  end;

  Criteria := TdormCriteria.NewCriteria('Age', Equal, 32);
  coll := Session.List<TPerson>(Criteria, true);
  try
    CheckEquals(1, coll.Count);
  finally
    coll.Free;
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
    Session.Save(p);
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
    Session.Save(p);
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

initialization

RegisterTest(TFrameworkTests.Suite);

end.
