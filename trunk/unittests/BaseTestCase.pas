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
  dorm,
  dorm.Collections;

type
  TBaseTestCase = class(TTestCase)
  protected
    Session: dorm.TSession;
    function GetDORMConfigFileName: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  ioutils,
  classes,
  dorm.Commons;

function TBaseTestCase.GetDORMConfigFileName: String;
begin
{$IFDEF INTERBASE_STRATEGY}
  result := 'dorm_interbase.conf';
{$ENDIF}
{$IFDEF FIREBIRD_STRATEGY}
  result := 'dorm_firebird.conf';
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
