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

unit TestDORMSpeed;

interface

uses
  TestFramework,
  dorm,
  Generics.Collections,
  BaseTestCase;

type
  TTestDORMSpeed = class(TBaseTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLotOfObjects;
    procedure TestList;
  end;

implementation

uses
  windows,
  Classes,
  SysUtils,
  dorm.Commons,
  dorm.Collections,
  dorm.tests.bo,
  DateUtils,
  dorm.InterposedObject;

{ TTestDORMSpeed }

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
  persone: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  start: TDateTime;
  cmp: TdormComparer<TPerson>;
const
  OBJECTS_NUMBER = 1000;
begin
  start := now;
  for I := 1 to OBJECTS_NUMBER do
  begin
    p := TPerson.NewPerson;
    p.FirstName := p.FirstName + Format('%4d', [I]);
    Session.Insert(p);
    p.Free;
  end;
  Session.Commit;
  Session.StartTransaction;
  persone := Session.LoadList<TPerson>;
  try
    CheckEquals(OBJECTS_NUMBER, persone.Count);
    cmp := TdormComparer<TPerson>.Create('FirstName');
    persone.Sort(cmp);
    cmp.Free;
    for I := 1 to 100 do
      CheckEquals('Daniele' + Format('%4d', [I]), TPerson(persone[I - 1])
        .FirstName);
    cmp := TdormComparer<TPerson>.Create('FirstName');
    persone.Sort(cmp);
    persone.Reverse;
    cmp.Free;
    for I := 1 to 100 do
      CheckEquals('Daniele' + Format('%4d', [I]), TPerson(persone[1000 - I])
        .FirstName);
    Session.Commit;
  finally
    persone.Free;
  end;
  CheckTrue(MilliSecondsBetween(now, start) < 35000,
    'Too slow: ' + inttostr(MilliSecondsBetween(now, start)));
end;

procedure TTestDORMSpeed.TestLotOfObjects;
var
  I: Integer;
  p: TPerson;
  start: TDateTime;
begin
  start := now;
  for I := 1 to 1000 do
  begin
    p := TPerson.NewPerson;
    p.FirstName := p.FirstName + Format('%4d', [I]);
    Session.Insert(p);
    p.Free;
  end;
  Session.Commit;
  CheckEquals(1000, Session.Count(TPerson));
  CheckTrue(MilliSecondsBetween(now, start) < 45000,
    'Too slow: ' + inttostr(MilliSecondsBetween(now, start)));
end;

initialization

RegisterTest(TTestDORMSpeed.Suite);

end.
