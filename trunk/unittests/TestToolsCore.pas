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

unit TestToolsCore;

interface

uses
  TestFramework,
  BaseTestCase,
  dorm.Finders,
  dorm,
  dorm.Collections, dorm.Filters, System.TypInfo, dorm.CodeGenerator.Delphi,
  dorm.CodeGenerator;

type
  TToolsCoreTests = class(TTestCase)
  published
    procedure TestDelphiCodeGenerator;
  end;

implementation

uses
  Generics.Collections, dorm.tests.bo, System.SysUtils, System.IOUtils,
  System.Classes;

{ TToolsCoreTests }

procedure TToolsCoreTests.TestDelphiCodeGenerator;
var
  dcg: TDelphiCodeGenerator;
  sr: TStreamReader;
begin
  dcg := TDelphiCodeGenerator.Create;
  try
    sr := TFile.OpenText('test.mapping.json');
    try
      dcg.Execute(sr, 'BusinnessObjects.pas');
    finally
      sr.Free;
    end;
  finally
    dcg.Free;
  end;
end;

initialization

RegisterTest(TToolsCoreTests.Suite);

end.
