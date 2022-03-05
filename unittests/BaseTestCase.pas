{ *******************************************************************************
  Copyright 2010-2015 Daniele Teti

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
  superobject;

type
  TBaseTestCase = class(TTestCase)
  protected
    Session: dorm.TSession;
    function CreateSession: dorm.TSession;
    function GetDORMConfigFileName: string;
    function GetDORMMappingFileName: string;
    function CreateRandomPeople:

    {$IF CompilerVersion > 22}TObjectList<TPerson>{$ELSE}TdormObjectList<TPerson>{$IFEND};

  public
    procedure OnBeforeConfigureStrategy(Sender: TObject;
      AAdapterConfiguration: ISuperObject);
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  ioutils,
  classes,
  dorm.Commons,
  System.SysUtils;

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

function TBaseTestCase.CreateSession: dorm.TSession;
begin
  Result := TSession.Create(deTest);
  Result.OnBeforeConfigureStrategy := OnBeforeConfigureStrategy;
  Result.Configure(TStreamReader.Create(GetDORMConfigFileName)
    , TStreamReader.Create(GetDORMMappingFileName));
  Result.StartTransaction;
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
  {$IFDEF FIREBIRD_CI}

  Result := 'dorm_firebird_uib_ci.conf';

  {$ENDIF}
  {$IFDEF FIREBIRD_ZEOSDBO_STRATEGY}

  Result := 'dorm_firebird_zeosdbo.conf';

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
  {$IFDEF SQLSERVER_FIREDAC_STRATEGY}

  Result := 'dorm_sqlserver_firedac.conf';

  {$ENDIF}
  {$IFNDEF INTERBASE_STRATEGY}
  {$IFNDEF INTERBASE_UIB_STRATEGY}
  {$IFNDEF FIREBIRD_STRATEGY}
  {$IFNDEF FIREBIRD_UIB_STRATEGY}
  {$IFNDEF FIREBIRD_ZEOSDBO_STRATEGY}
  {$IFNDEF SQLITE3_STRATEGY}
  {$IFNDEF SQLSERVER_STRATEGY}
  {$IFNDEF SQLSERVER_DEVART_STRATEGY}
  {$IFNDEF FIREBIRD_CI}
  {$IFNDEF CONSOLE_TESTRUNNER}
  {$IFNDEF SQLSERVER_FIREDAC_STRATEGY}
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
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}

end;

function TBaseTestCase.GetDORMMappingFileName: string;
begin
  Result := 'dorm_tests.mapping';
end;

procedure TBaseTestCase.OnBeforeConfigureStrategy(Sender: TObject;
  AAdapterConfiguration: ISuperObject);
begin

  {$IFDEF FIREBIRD_CI}

  AAdapterConfiguration.S['database_connection_string'] := 'localhost:' +
    ExtractFilePath(ParamStr(0)) + '..\Samples\Data\DORM_TEST.FDB';

  {$ENDIF}

end;

procedure TBaseTestCase.SetUp;
begin
  inherited;
  Session := CreateSession;
  Session.DeleteAll(TPerson);
  Session.DeleteAll(TPhone);
  Session.DeleteAll(TEmail);
  Session.DeleteAll(TCar);
  Session.DeleteAll(TToDo);
  Session.Commit;
  Session.StartTransaction;
end;

procedure TBaseTestCase.TearDown;
begin
  Session.Free;
  inherited;
end;

end.
