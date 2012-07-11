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

unit FindersTests;

interface

uses
  TestFramework,
  BaseTestCase,
  dorm.Finders,
  dorm,
  dorm.Collections,
  dorm.Filters,
  TypInfo;

type
  TFindersTests = class(TBaseTestCase)
  published
    procedure TestSimpleFinder;
  end;

  TPeopleFinder = class(TdormCriteria, ICustomCriteria)
  private
    FPersonName: string;
  public
    constructor Create(APersonName: string);
    function GetSQL: string;
    function GetItemClassInfo: PTypeInfo;
  end;

implementation

uses
  Generics.Collections,
  dorm.tests.bo,
  dorm.InterposedObject,
  SysUtils;

{ TFindersTests }

procedure TFindersTests.TestSimpleFinder;
var
  Coll: {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  P: TPerson;
begin
  Session.DeleteAll(TPerson);
  Coll := {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND}.Create;
  try
    Session.FillList<TPerson>(Coll, TPeopleFinder.Create('Daniele'));
    CheckEquals(0, Coll.Count);
    P := TPerson.NewPerson;
    try
      P.FirstName := 'Daniele';
      Session.Persist(P);
    finally
      P.Free;
    end;
    Session.FillList<TPerson>(Coll, TPeopleFinder.Create('Daniele'));
    CheckEquals(1, Coll.Count);
    Session.FillList<TPerson>(Coll, TPeopleFinder.Create('WrongName'));
    CheckEquals(0, Coll.Count, 'WrongName returns data!');
  finally
    Coll.Free;
  end;
end;

{ TPeopleFinder }

constructor TPeopleFinder.Create(APersonName: string);
begin
  inherited Create;
  FPersonName := APersonName;
end;

function TPeopleFinder.GetItemClassInfo: PTypeInfo;
begin
  Result := TypeInfo(TPerson);
end;

function TPeopleFinder.GetSQL: string;
begin
  Result :=
    Format('SELECT * FROM PEOPLE WHERE FIRST_NAME = ''%s''',
    [StringReplace(FPersonName, '''', '''''', [rfReplaceAll])]);
end;

initialization

RegisterTest(TFindersTests.Suite);

end.
