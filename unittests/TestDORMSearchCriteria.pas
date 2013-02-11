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

unit TestDORMSearchCriteria;

interface

uses
  TestFramework,
  TypInfo,
  dorm,
  dorm.Filters,
  dorm.Commons,
  Generics.Collections,
  BaseTestCase;

type
  TTestDORMSearchCriteria = class(TBaseTestCase)
  published
    procedure TestSimpleRawCriteria;
    procedure TestSearchByAttributes;
    procedure TestSearchByAttributesAll;
    procedure TestNestedCriteria;
    procedure TestSearchByAttributesWithNestedCriteria;
  end;

  TTestDORMQueryable = class(TBaseTestCase)
  published
    procedure TestSelectListWithHardCoded;
    procedure TestSelectListWithParams;
    procedure TestExecSimpleSelectAllFromTable;
    procedure TestCustomSearchCriteriaWithParams;
  end;

implementation

uses
  Rtti,
  dorm.tests.bo,
  dorm.Collections,
  SysUtils, dorm.Query, dorm.tests.objstatus.bo, dorm.Utils;

{ TTestDORMSearchCriteria }

procedure TTestDORMSearchCriteria.TestNestedCriteria;
var
  Crit1: ICriteria;
  Crit2: ICriteria;
  CriteriaOr, CriteriaAnd: ICriteria;
begin
  Crit1 := NewCriteria('LastName', coEqual, 'Smith')
    ._Or('LastName', coEqual, 'Teti');
  Crit2 := NewCriteria('LastName', coEqual, 'Smith')
    ._Or('LastName', coEqual, 'Teti');
  CriteriaOr := NewCriteria(Crit1)._Or(Crit2);
  CriteriaAnd := NewCriteria(Crit1)._Or(Crit2);
  CheckEquals(2, Crit1.Count, 'Crit1 doesn''t contain 2 elements');
  CheckEquals(2, Crit2.Count, 'Crit2 doesn''t contain 2 elements');
  CheckEquals(2, CriteriaOr.Count, 'Criteria doesn''t contain 2 elements');
  CheckEquals(2, CriteriaOr.GetCriteria(0).Count,
    'Criteria.GetCriteria(0) doesn''t contain 2 elements');
  CheckEquals(2, CriteriaOr.GetCriteria(1).Count,
    'Criteria.GetCriteria(1) doesn''t contain 2 elements');
  CheckEquals(2, CriteriaAnd.Count, 'Criteria doesn''t contain 2 elements');
  CheckEquals(2, CriteriaAnd.GetCriteria(0).Count,
    'Criteria.GetCriteria(0) doesn''t contain 2 elements');
  CheckEquals(2, CriteriaAnd.GetCriteria(1).Count,
    'Criteria.GetCriteria(1) doesn''t contain 2 elements');
end;

procedure TTestDORMSearchCriteria.TestSearchByAttributes;
var
  Criteria: ICriteria;
  People: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  p: TPerson;
begin
  Session.DeleteAll(TPerson);
  p := TPerson.NewPerson;
  p.LastName := 'Smith';
  Session.InsertAndFree(p);
  p := TPerson.NewPerson;
  Session.InsertAndFree(p);

  Criteria := TdormCriteria.NewCriteria('FirstName',
    TdormCompareOperator.coEqual,
    'Daniele')._And('LastName', TdormCompareOperator.coNotEqual, 'Smith');
  People := Session.LoadList<TPerson>(Criteria);
  try
    CheckEquals(1, People.Count);
  finally
    People.Free;
  end;
end;

procedure TTestDORMSearchCriteria.TestSearchByAttributesAll;
var
  p: TPerson;
  crit: ICriteria;
  List: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
begin
  p := TPerson.NewPerson;
  try
    p.BornDate := EncodeDate(2000, 10, 20);
    p.BornTimeStamp := p.BornDate + EncodeTime(12, 10, 5, 0);
    Session.Insert(p);
  finally
    p.Free;
  end;
  crit := TdormCriteria.NewCriteria('BornDate', TdormCompareOperator.coEqual,
    EncodeDate(2000, 10, 20));
  List := Session.LoadList<TPerson>(crit);
  try
    CheckEquals(1, List.Count);
  finally
    List.Free;
  end;

  crit := TdormCriteria.NewCriteria('BornTimeStamp',
    TdormCompareOperator.coEqual,
    EncodeDate(2000, 10, 20) + EncodeTime(12, 10, 5, 0));
  List := Session.LoadList<TPerson>(crit);
  try
    CheckEquals(1, List.Count);
  finally
    List.Free;
  end;

  crit := TdormCriteria
    .NewCriteria('BornTimeStamp', TdormCompareOperator.coGreaterOrEqual,
    EncodeDate(2000, 10, 20) + EncodeTime(12, 10, 5, 0))
    ._And('BornDate', TdormCompareOperator.coLowerOrEqual,
    EncodeDate(2000, 10, 20));
  List := Session.LoadList<TPerson>(crit);
  try
    CheckEquals(1, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestDORMSearchCriteria.TestSearchByAttributesWithNestedCriteria;
var
  Criteria: ICriteria;
  People: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  p: TPerson;
  Crit1: ICriteria;
  Crit2: ICriteria;
begin
  Session.DeleteAll(TPerson);
  p := TPerson.NewPerson;
  p.LastName := 'Smith';
  Session.InsertAndFree(p);
  p := TPerson.NewPerson;
  Session.InsertAndFree(p);

  // where FirstName = 'Daniele' and (LastName = 'Smith' or LastName = 'Teti')
  Criteria := NewCriteria('FirstName', TdormCompareOperator.coEqual, 'Daniele')
    ._And(NewCriteria('LastName', coEqual, 'Smith')._Or('LastName',
    coEqual, 'Teti'));
  People := Session.LoadList<TPerson>(Criteria);
  try
    CheckEquals(2, People.Count);
  finally
    People.Free;
  end;

  // where (LastName = 'Smith' and LastName = 'Teti') or FirstName = 'Daniele'
  Criteria := NewCriteria(NewCriteria('LastName', coEqual, 'Smith')
    ._And('LastName', coEqual,
    'Teti'))._Or('FirstName', coEqual, 'Daniele');
  People := Session.LoadList<TPerson>(Criteria);
  try
    CheckEquals(2, People.Count);
  finally
    People.Free;
  end;

  // where (LastName = 'Smith' or LastName = 'Teti') or (FirstName = 'Unknown' or FirstName = 'Daniele')

  Crit1 := NewCriteria('LastName', coEqual, 'Smith')
    ._Or('LastName', coEqual, 'Teti');
  Crit2 := NewCriteria('LastName', coEqual, 'Smith')
    ._Or('LastName', coEqual, 'Teti');
  Criteria := NewCriteria(Crit1)._Or(Crit2);

  People := Session.LoadList<TPerson>(Criteria);
  try
    CheckEquals(2, People.Count);
  finally
    People.Free;
  end;
end;

procedure TTestDORMSearchCriteria.TestSimpleRawCriteria;
var
  intf: ICriteria;
  People: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  SQL: string;
begin
  Session.DeleteAll(TPerson);
  SQL := 'SELECT * FROM PEOPLE WHERE LAST_NAME = ''TETI''';
  intf := TSimpleFinder.Create(TypeInfo(TPerson), SQL);
  People := Session.LoadList<TPerson>(intf);
  try
    CheckEquals(0, People.Count);
  finally
    People.Free;
  end;

  Session.InsertAndFree(TPerson.NewPerson);
  SQL := 'SELECT * FROM PEOPLE';
  intf := TSimpleFinder.Create(TypeInfo(TPerson), SQL);
  People := Session.LoadList<TPerson>(intf);
  try
    CheckEquals(1, People.Count);
  finally
    People.Free;
  end;
end;

{ TTestDORMQueryable }

procedure TTestDORMQueryable.TestCustomSearchCriteriaWithParams;
var
//  intf: ICustomCriteria;
  mArguments: TObjectList<TdormParameter>;
//  cc: TSQLCustomCriteria;
  SQL: string;
  mScope: TDictionary<string, TRttiType>;
begin
  mArguments := TObjectList<TdormParameter>.Create(true);
  mArguments.Add(TdormParameter.Create(TypeInteger, 1));
  mArguments.Add(TdormParameter.Create(TypeString, 'daniele'));
  mScope := TDictionary<string, TRttiType>.Create;
  mScope.Add('TPersonOS', TdormUtils.ctx.GetType(TPersonOS));
  SQL := TDSQLParser.Parse
    ('select * from tablename where #TPersonOS.ID# = ? and #TPersonOS.FirstName# = ?',
    Session.GetMapping,
    Session.Strategy,
    mScope,
    mArguments);
  mArguments.Free;
//  cc.Free;
  mScope.Free;
  CheckEquals
    ('select * from tablename where ID = 1 and FIRST_NAME = ''daniele''', SQL);
end;

procedure TTestDORMQueryable.TestExecSimpleSelectAllFromTable;
var
  List: TPeopleOS;
  p: TPersonOS;
begin
  List := TPeopleOS.Create(true);
  try
    Session.FillListSQL<TPersonOS>(List, Select.From(TPersonOS));
    CheckEquals(0, List.Count);
    List.Clear;
    p := TPersonOS.NewPerson;
    Session.Persist(p);
    p.Free;
    Session.FillListSQL<TPersonOS>(List, Select.From(TPersonOS));
    CheckEquals(1, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestDORMQueryable.TestSelectListWithHardCoded;
var
  List: TObjectList<TPersonOS>;
  p: TPersonOS;
  pid: Integer;
begin
  List := TObjectList<TPersonOS>.Create(true);
  try
    p := TPersonOS.NewPerson;
    p.FirstName := 'The First';
    Session.Persist(p);
    Session.ClearOID(p);
    p.FirstName := 'The Second';
    Session.Persist(p);
    pid := p.ID;
    p.Free;
    Session.FillListSQL<TPersonOS>(List, Select().From(TPersonOS));
    CheckEquals(2, List.Count);
    Session.FillListSQL<TPersonOS>(List, Select().From(TPersonOS)
      .Where('#TPersonOS.ID# = ' + inttostr(pid)));
    CheckEquals(1, List.Count);
    Session.FillListSQL<TPersonOS>(List, Select().From(TPersonOS)
      .orderBy('#TPersonOS.ID#'));
    CheckEquals(2, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestDORMQueryable.TestSelectListWithParams;
var
  List: TObjectList<TPersonOS>;
  p: TPersonOS;
  pid: Integer;
begin
  List := TObjectList<TPersonOS>.Create(true);
  try
    p := TPersonOS.NewPerson;
    p.FirstName := 'The First';
    Session.Persist(p);
    Session.ClearOID(p);
    p.FirstName := 'The Second';
    Session.Persist(p);
    pid := p.ID;
    p.Free;
    Session.FillListSQL<TPersonOS>(List, Select().From(TPersonOS)
      .Where('id = ?', [pid]));
    CheckEquals(1, List.Count);
    List.Clear;
    Session.FillListSQL<TPersonOS>(List, Select().From(TPersonOS)
      .Where('#TPersonOS.FirstName# = ? or #TPersonOS.FirstName# = ?',
      ['The First', 'The Second']).orderBy('#TPersonOS.ID#'));
    CheckEquals(2, List.Count);
    CheckEquals('The First', List[0].FirstName);
    CheckEquals('The Second', List[1].FirstName);
  finally
    List.Free;
  end;
end;

initialization

RegisterTest(TTestDORMSearchCriteria.Suite);
RegisterTest(TTestDORMQueryable.Suite);

finalization

end.
