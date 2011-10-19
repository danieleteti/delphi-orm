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
  protected
    function GetDORMConfigFileName: String; override;
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

function TFrameworkTests.GetDORMConfigFileName: String;
begin
  result := 'dorm.conf';
end;

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
  oid: Integer;
  md5before, md5after: string;
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
  oid: Integer;
  md5before, md5after: string;
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
  Coll: TdormCollection;
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
    Coll := Session.List<TPerson>(Criteria, false);
    try
      CheckEquals(1, Coll.Count);
      m_id := TPerson(Coll.GetItem(0)).ID;
    finally
      Coll.Free;
    end;

    Criteria.Add('ID', Equal, m_id);
    Coll := Session.List<TPerson>(Criteria, false);
    try
      CheckEquals(1, Coll.Count);
    finally
      Coll.Free;
    end;

    Criteria.Add('ID', GreaterThan, m_id);
    Coll := Session.List<TPerson>(Criteria, false);
    try
      CheckEquals(0, Coll.Count);
    finally
      Coll.Free;
    end;
  finally
    Criteria.Free;
  end;
end;

procedure TFrameworkTests.TestLoadByAttributeWithRelationOperators;
var
  Coll: TdormCollection;
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
  Coll := Session.List<TPerson>(Criteria, true);
  try
    CheckEquals(2, Coll.Count);
  finally
    Coll.Free;
  end;

  Criteria := TdormCriteria.NewCriteria('Age', Equal, 32);
  Coll := Session.List<TPerson>(Criteria, true);
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
