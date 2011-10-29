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

unit TestDORMSearchCriteria;

interface

uses
  TestFramework,
  TypInfo,
  dorm,
  dorm.Commons, BaseTestCase;

type
  TTestDORMSearchCriteria = class(TBaseTestCase)
  published
    procedure TestSimpleRawCriteria;
    procedure TestSearchByAttributes;
  end;

implementation

uses
  Rtti, dorm.tests.bo, dorm.Collections;

{ TTestDORMSearchCriteria }

procedure TTestDORMSearchCriteria.TestSearchByAttributes;
var
  Criteria: TdormCriteria;
  People: TdormCollection;
  p: TPerson;
begin
  Session.DeleteAll(TPerson);
  p := TPerson.NewPerson;
  p.LastName := 'Smith';
  Session.SaveAndFree(p);
  p := TPerson.NewPerson;
  Session.SaveAndFree(p);

  Criteria := TdormCriteria.
    NewCriteria('FirstName', TdormCompareOperator.Equal, 'Daniele').
    Add('LastName', TdormCompareOperator.Different, 'Smith');
  People := Session.List<TPerson>(Criteria);
  try
    CheckEquals(1, People.Count);
  finally
    People.Free;
  end;
end;

procedure TTestDORMSearchCriteria.TestSimpleRawCriteria;
var
  intf: IdormSearchCriteria;
  People: TdormCollection;
  SQL: string;
begin
  Session.DeleteAll(TPerson);

  SQL := 'SELECT * FROM PEOPLE';
  intf := TdormSimpleSearchCriteria.Create(TypeInfo(TPerson), SQL);
  People := Session.List(intf);
  try
    CheckEquals(0, People.Count);
  finally
    People.Free;
  end;

  Session.SaveAndFree(TPerson.NewPerson);
  SQL := 'SELECT * FROM PEOPLE';
  intf := TdormSimpleSearchCriteria.Create(TypeInfo(TPerson), SQL);
  People := Session.List(intf);
  try
    CheckEquals(1, People.Count);
  finally
    People.Free;
  end;
end;

initialization

RegisterTest(TTestDORMSearchCriteria.Suite);

finalization

end.
