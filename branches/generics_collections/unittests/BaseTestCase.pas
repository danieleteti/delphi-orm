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

unit BaseTestCase;

interface

uses
  TestFramework,
  Generics.Collections,
  dorm,
  dorm.Collections, dorm.tests.bo;

type
  TBaseTestCase = class(TTestCase)
  protected
    Session: dorm.TSession;
    function GetDORMConfigFileName: String;
    function CreateRandomPeople: TObjectList<TPerson>;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  ioutils,
  classes,
  dorm.Commons;

function TBaseTestCase.CreateRandomPeople: TObjectList<TPerson>;
var
  p: TPerson;
begin
  Result := TObjectList<TPerson>.Create;
  Result.Add(TPerson.NewPerson);
  p := TPerson.Create;
  p.FirstName := 'Scott';
  p.LastName := 'Summers';
  p.Age := 30;
  Result.Add(p);
  p := TPerson.Create;
  p.FirstName := 'Sue';
  p.LastName := 'Storm';
  p.Age := 28;
  Result.Add(p);
  p := TPerson.Create;
  p.FirstName := 'Bruce';
  p.LastName := 'Banner';
  p.Age := 50;
  Result.Add(p);
  p := TPerson.Create;
  p.FirstName := 'Reed';
  p.LastName := 'Richards';
  p.Age := 35;
  Result.Add(p);
end;

function TBaseTestCase.GetDORMConfigFileName: String;
begin
{$IFDEF INTERBASE_STRATEGY}
  Result := 'dorm_interbase.conf';
{$ENDIF}
{$IFDEF FIREBIRD_STRATEGY}
  Result := 'dorm_firebird.conf';
{$ENDIF}
{$IFNDEF INTERBASE_STRATEGY}
{$IFNDEF FIREBIRD_STRATEGY}
{$MESSAGE ERROR '************************************************'}
{$MESSAGE ERROR '**>>> There are not strategy conditionals defined'}
{$MESSAGE ERROR '************************************************'}
{$ENDIF}
{$ENDIF}
end;

procedure TBaseTestCase.SetUp;
begin
  inherited;
  Session := TSession.Create(deTest);
  Session.Configure(TStreamReader.Create(GetDORMConfigFileName));
  Session.StartTransaction;
end;

procedure TBaseTestCase.TearDown;
begin
  Session.Free;
  inherited;
end;

end.
