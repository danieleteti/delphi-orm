unit TestDORMSearchCriteria;

interface

uses
  TestFramework,
  TypInfo,
  dorm,
  dorm.Commons;

type
  TTestDORMSearchCriteria = class(TTestCase)
  published
    procedure TestSimpleRawCriteria;
  end;

implementation

uses
  Rtti, dorm.tests.bo;

{ TTestDORMSearchCriteria }

procedure TTestDORMSearchCriteria.TestSimpleRawCriteria;
var
  intf: IdormSearchCriteria;
const
  SQL = 'SELECT * FROM PEOPLE';
begin
  intf := TdormSimpleSearchCriteria.Create(TypeInfo(TPerson), SQL);
  CheckEquals(PTypeInfo(TypeInfo(TPerson))^.name, intf.GetItemClassInfo.name);
  CheckEquals(SQL, intf.GetSQL);
end;

initialization

RegisterTest(TTestDORMSearchCriteria.Suite);

finalization

end.
