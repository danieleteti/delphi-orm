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

unit BaseTestCase;

interface

uses
  TestFramework,
  Generics.Collections,
  dorm,
  dorm.Collections,
  dorm.tests.bo,
  dorm.InterposedObject;

type
  TBaseTestCase = class(TTestCase)
  protected
    Session: dorm.TSession;
    function GetDORMConfigFileName: string;
    function GetDORMMappingFileName: string;
    function CreateRandomPeople:
{$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  ioutils,
  classes,
  dorm.Commons;

function TBaseTestCase.CreateRandomPeople:
{$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};
var
  p: TPerson;
begin
  Result := {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND}.Create;
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

function TBaseTestCase.GetDORMConfigFileName: string;
begin
{$IFDEF INTERBASE_STRATEGY}
  Result := 'dorm_interbase.conf';
{$ENDIF}
{$IFDEF INTERBASE_UIB_STRATEGY}
  Result := 'dorm_interbase_uib.conf';
{$ENDIF}
{$IFDEF FIREBIRD_STRATEGY}
  Result := 'dorm_firebird.conf';
{$ENDIF}
{$IFDEF FIREBIRD_UIB_STRATEGY}
  Result := 'dorm_firebird_uib.conf';
{$ENDIF}
{$IFDEF CONSOLE_TESTRUNNER}
  Result := 'dorm_firebird_uib_ci.conf';
{$ENDIF}
{$IFDEF SQLSERVER_STRATEGY}
  Result := 'dorm_sqlserver.conf';
{$ENDIF}
{$IFDEF SQLSERVER_DEVART_STRATEGY}
  Result := 'dorm_sqlserver_devart.conf';
{$ENDIF}
{$IFDEF SQLITE3_STRATEGY}
  Result := 'dorm_sqlite3.conf';
{$ENDIF}
{$IFNDEF INTERBASE_STRATEGY}
{$IFNDEF INTERBASE_UIB_STRATEGY}
{$IFNDEF FIREBIRD_STRATEGY}
{$IFNDEF FIREBIRD_UIB_STRATEGY}
{$IFNDEF SQLITE3_STRATEGY}
{$IFNDEF SQLSERVER_STRATEGY}
{$IFNDEF SQLSERVER_DEVART_STRATEGY}
{$IFNDEF CONSOLE_TESTRUNNER}
{$MESSAGE ERROR '**************************************************'}
{$MESSAGE ERROR '**>>> There are not strategy conditionals defined '}
{$MESSAGE ERROR '**>>> You should select a REAL BUILD configuration'}
{$MESSAGE ERROR '**************************************************'}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function TBaseTestCase.GetDORMMappingFileName: string;
begin
  Result := 'dorm_tests.mapping';
end;

procedure TBaseTestCase.SetUp;
begin
  inherited;
  Session := TSession.CreateSession(deTest);
  Session.Configure(
    TStreamReader.Create(GetDORMConfigFileName),
    TStreamReader.Create(GetDORMMappingFileName));
  Session.StartTransaction;
  Session.DeleteAll(TPerson);
  Session.DeleteAll(TPhone);
  Session.DeleteAll(TEmail);
  Session.DeleteAll(TCar);
  Session.Commit;
  Session.StartTransaction;
end;

procedure TBaseTestCase.TearDown;
begin
  Session.Free;
  inherited;
end;

end.
