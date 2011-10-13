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
    procedure TestTDateTime;
    procedure TestBlob;
  end;

implementation

uses
  Classes,
  SysUtils,
  DateUtils,
  dorm.Utils, dorm.tests.bo;

{ TFrameworkTests }

function TFrameworkTests.GetDORMConfigFileName: String;
begin
  Result := 'dorm.conf';
end;

procedure TFrameworkTests.TestBlob;
// var
// i: TInvioPreventivo;
// dt: TDateTime;
// i_id: Int64;
// stream: TStreamReader;
// s: string;
begin
  // dt := EncodeDateTime(2011, 10, 30, 12, 30, 30, 0);
  // i := TInvioPreventivo.Create;
  // try
  // i.IDPreventivo := 1234;
  // i.DataInvio := dt;
  // // i.Allegato := TStringStream.Create('Hello World');
  // // i.Note := 'Sono una nota'; //TEXT field_type still not suported. Use blob instead
  // Session.Save(i);
  // i_id := i.ID;
  // finally
  // i.Free;
  // end;
  //
  // i := Session.Load<TInvioPreventivo>(i_id);
  // try
  // CheckEquals(dt, i.DataInvio);
  // // stream := TStreamReader.Create(i.Allegato);
  // // try
  // // s := stream.ReadToEnd;
  // // finally
  // // stream.Free;
  // // end;
  // // CheckEquals('Hello World', s);
  // // CheckEquals('Sono una nota', i.Note);
  // Session.Delete(i);
  // finally
  // i.Free;
  // end;
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
// var
// all: TAllegatoRiga;
// all2: TAllegatoRiga;
begin
  // all := TAllegatoRiga.CreateBy('Test.pdf', 2);
  // try
  // all2 := TdormUtils.Clone(all) as TAllegatoRiga;
  // try
  // CheckEquals(all.IDRiga, all2.IDRiga);
  // CheckEquals(all.FileName, all2.FileName);
  // CheckEquals(all.FullName, all2.FullName);
  // CheckEquals(all.Allegato.Size, all2.Allegato.Size);
  // CheckEquals(all.Allegato.Position, all2.Allegato.Position);
  // CheckFalse(all.Allegato = all2.Allegato);
  // finally
  // all2.Free;
  // end;
  // finally
  // all.Free;
  // end;
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

  Criteria := TdormCriteria
    .NewCriteria('FirstName', Equal, 'Daniele')
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

procedure TFrameworkTests.TestTDateTime;
// var
// i: TInvioPreventivo;
// dt: TDateTime;
// i_id: Int64;
begin
  // dt := EncodeDateTime(2011, 10, 30, 12, 30, 30, 0);
  //
  // i := TInvioPreventivo.Create;
  // try
  // i.IDPreventivo := 1234;
  // i.DataInvio := dt;
  // Session.Save(i);
  // i_id := i.ID;
  // finally
  // i.Free;
  // end;
  //
  // i := Session.Load<TInvioPreventivo>(i_id);
  // try
  // CheckEquals(dt, i.DataInvio);
  // Session.Delete(i);
  // finally
  // i.Free;
  // end;
end;

initialization

RegisterTest(TFrameworkTests.Suite);

end.
