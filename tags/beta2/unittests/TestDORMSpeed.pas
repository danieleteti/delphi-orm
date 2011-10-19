unit TestDORMSpeed;

interface

uses
  TestFramework, dorm, BaseTestCase;

type
  TTestDORMSpeed = class(TBaseTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  protected
    function GetDORMConfigFileName: String; override;
  published
    procedure TestLotOfObjects;
    procedure TestList;
  end;

implementation

uses
  Classes,
  SysUtils,
  dorm.Commons, dorm.Collections, dorm.tests.bo;

{ TTestDORMSpeed }

function TTestDORMSpeed.GetDORMConfigFileName: String;
begin
  Result := 'dorm.conf';
end;

procedure TTestDORMSpeed.SetUp;
begin
  inherited;
  Session.DeleteAll(TPerson);
end;

procedure TTestDORMSpeed.TearDown;
begin
  Session.Free;
end;

procedure TTestDORMSpeed.TestList;
var
  I: Integer;
  p: TPerson;
  persone: TdormCollection;
begin
  for I := 1 to 1000 do
  begin
    p := TPerson.NewPerson;
    p.FirstName := p.FirstName + Format('%4d', [I]);
    Session.Save(p);
    p.Free;
  end;
  Session.Commit;
  Session.StartTransaction;
  persone := Session.ListAll<TPerson>;
  try
    CheckEquals(1000, persone.Count);
    Session.Commit;
  finally
    persone.Free;
  end;
end;

procedure TTestDORMSpeed.TestLotOfObjects;
var
  I: Integer;
  p: TPerson;
begin
  for I := 1 to 1000 do
  begin
    p := TPerson.NewPerson;
    p.FirstName := p.FirstName + Format('%4d', [I]);
    Session.Save(p);
    p.Free;
  end;
  Session.Commit;
  CheckEquals(1000, Session.Count(TPerson));
end;

initialization

RegisterTest(TTestDORMSpeed.Suite);

end.
