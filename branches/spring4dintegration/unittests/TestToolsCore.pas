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

unit TestToolsCore;

interface

uses
  TestFramework,
  BaseTestCase,
  dorm.Finders,
  dorm,
  dorm.Collections,
  dorm.Filters,
  System.TypInfo,
  dorm.CodeGenerator.Delphi,
  dorm.CodeGenerator;

type
  TToolsCoreTests = class(TTestCase)
  private
    Session: TSession;
    function GetDORMConfigFileName: string;
    function GetDORMMappingFileName: string;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestBuildDataBase;
  end;

implementation

uses
  Generics.Collections,
  dorm.tests.bo,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  dorm.Commons,
  dorm.DBCreator.Sqlite3;

{ TToolsCoreTests }

function TToolsCoreTests.GetDORMConfigFileName: string;
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

function TToolsCoreTests.GetDORMMappingFileName: string;
begin
  Result := 'dorm_tests.mapping';
end;

procedure TToolsCoreTests.SetUp;
begin
  inherited;
  Session := TSession.CreateSession(deTest);
  Session.Configure(
    TStreamReader.Create(GetDORMConfigFileName),
    TStreamReader.Create(GetDORMMappingFileName));
end;

procedure TToolsCoreTests.TearDown;
begin
  Session.Free;
  inherited;
end;

procedure TToolsCoreTests.TestBuildDataBase;
begin
  Session.BuildDatabase;
end;

initialization

RegisterTest(TToolsCoreTests.Suite);

end.
