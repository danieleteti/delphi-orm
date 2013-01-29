unit TestDORMObjStatus;

interface

uses
  TestFramework, dorm.Commons, BaseTestCase;

type
  TTestObjStatus = class(TBaseTestCase)
  public
    procedure Setup; override;
    procedure TearDown; override;

  published
    procedure TestDirtyLifeCycle;
  end;

implementation

uses
  dorm.Utils, System.Rtti, System.SysUtils, System.Classes,
  dorm.tests.objstatus.bo, System.DateUtils, dorm.ObjectStatus;

{ TTestDuckTyping }

procedure TTestObjStatus.Setup;
begin
  inherited;

end;

procedure TTestObjStatus.TearDown;
begin
  inherited;

end;

{ todo: this test is really really too big. I've to refactor in a million of other test methods }
procedure TTestObjStatus.TestDirtyLifeCycle;
var
  p: TPersonOS;
  ID: Integer;
  p1: TPersonOS;
  phone1: TPhoneOS;
  Phone1ID: Integer;
  Phone2ID: Integer;
  Car: TCarOS;
  p2: TPersonOS;
begin
  p := TPersonOS.Create;
  try
    CheckTrue(p.objstatus = osDirty);
    Session.SetObjectStatus(p, osClean);
    CheckTrue(p.objstatus = osClean);
    Session.SetObjectStatus(p, osUnknown);
    CheckTrue(p.objstatus = osUnknown);
    Session.SetObjectStatus(p, osDeleted);
    CheckTrue(p.objstatus = osDeleted);
  finally
    p.Free;
  end;

  p := TPersonOS.Create;
  try
    p.Car.Free;
    p.Car := TCarOS.Create;
    // I'm too lazy to creanote another TPhone with a differetn TCar type

    p.Email.Free;
    p.Email := TEmailOS.Create;
    p.Email.Value := 'daniele.teti@gmail.com';

    p.FirstName := 'Daniele';
    p.LastName := 'Teti';
    p.Age := 32;
    p.BornDate := EncodeDate(1979, 11, 4);
    p.BornTimeStamp := EncodeDateTime(1979, 11, 4, 16, 10, 00, 0);
    phone1 := TPhoneOS.Create;
    phone1.Number := '324-678654';
    phone1.Model := 'Samsung Galaxy S';
    p.Phones.Add(phone1);
    phone1 := TPhoneOS.Create;
    phone1.Number := '876-12121212';
    phone1.Model := 'Samsung Galaxy S III';
    p.Phones.Add(phone1);
    Session.Persist(p); // insert object and his relations
    ID := p.ID;
    Phone1ID := TPhoneOS(p.Phones.Items[0]).ID;
    Phone2ID := TPhoneOS(p.Phones.Items[1]).ID;

    CheckTrue(p.objstatus = osClean);
    CheckTrue(TPhoneOS(p.Phones.Items[0]).objstatus = osClean);
    CheckTrue(TPhoneOS(p.Phones.Items[1]).objstatus = osClean);

    phone1 := Session.Load<TPhoneOS>(Phone1ID);
    CheckNotNull(phone1);
    phone1.Free;

    phone1 := Session.Load<TPhoneOS>(Phone2ID);
    CheckNotNull(phone1);
    phone1.Free;

    Session.Persist(p); // do nothing
    CheckTrue(p.objstatus = osClean);
    CheckTrue(TPhoneOS(p.Phones.Items[0]).objstatus = osClean);
    CheckTrue(TPhoneOS(p.Phones.Items[1]).objstatus = osClean);
    CheckEquals(1, Session.Count(TPersonOS));

    p.Phones[0].Model := 'changed model';
    p.Phones[0].objstatus := osDirty;
    Session.Persist(p); // do nothing
    phone1 := Session.Load<TPhoneOS>(Phone1ID);
    CheckNotNull(phone1);
    CheckEquals('changed model', phone1.Model);
    phone1.Free;

    // check the has one relation
    p.Car.Brand := 'Ford';
    p.Car.Model := 'Focus';
    p.Car.objstatus := osDirty;
    Session.Persist(p);

    Car := Session.Load<TCarOS>(p.Car.ID);
    CheckNotNull(Car);
    Car.Free;

    // One last check
    Session.Commit;
    Session.StartTransaction;
    p2 := Session.Load<TPersonOS>(p.ID);
    try
      CheckEquals(2, p2.Phones.Count);
      CheckEquals('Ford', p2.Car.Brand);
      CheckEquals('Focus', p2.Car.Model);
      CheckEquals('daniele.teti@gmail.com', p2.Email.Value);
    finally
      p2.Free;
    end;

    p.objstatus := osDeleted;
    Session.Persist(p); // delete person and related objects
    p1 := Session.Load<TPersonOS>(ID);
    CheckNull(p1, 'Person is not deleted');
    Session.Commit;
    CheckEquals(0, Session.Count(TPersonOS));
  finally
    p.Free;
  end;
end;

initialization

RegisterTest(TTestObjStatus.Suite);

end.
