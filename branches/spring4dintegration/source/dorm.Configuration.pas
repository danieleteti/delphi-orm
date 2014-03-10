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

unit dorm.Configuration;

interface

uses
  RTTI,
  classes,
  SysUtils,
  generics.Collections,
  superobject;

type
  TdormEnvironmentType = (etDevelopment, etTest, etRelease);

  TdormEnvironment = record
    Environment: TdormEnvironmentType;
    EnvironmentName: string;
    DatabaseAdapter: string;
    Password: String;
    HostName: String;
    UserName: String;
    DBConnectionString: String;
    KeyType: string;
    KeysGenerator: String;
    NullKeyValue: TValue;
    Active: boolean;
    procedure ParseEnvironmentConfiguration(json: ISuperObject;
      envType: TdormEnvironmentType; envName: string);
  end;

  TdormConfiguration = class
  private
    FLogClass: string;
    FConfigurationSO: ISuperObject;
    FMapping: ISuperObject;
    FEnvironment: TdormEnvironment;
    FPersistentClasses: TList<String>;
    function GetEnvName(envType: TdormEnvironmentType): string;
    procedure Configure(envType: TdormEnvironmentType);
  public
    constructor Create(TextReader: TTextReader; envType: TdormEnvironmentType);
    destructor Destroy; override;
    property LogClass: string read FLogClass;
    property Mapping: ISuperObject read FMapping;
    property PersistentClasses: TList<String> read FPersistentClasses;
    property Environment: TdormEnvironment read FEnvironment;
  end;

implementation

{ TdormConfiguration }

constructor TdormConfiguration.Create(TextReader: TTextReader;
  envType: TdormEnvironmentType);
var
  s: string;
begin
  inherited Create;
  FPersistentClasses := TList<String>.Create;
  try
    s := TextReader.ReadToEnd;
  finally
    TextReader.Free;
  end;
  FConfigurationSO := TSuperObject.ParseString(pwidechar(s), true);
  if not assigned(FConfigurationSO) then
    raise Exception.Create('Cannot parse configuration');
  Configure(envType);
end;

destructor TdormConfiguration.Destroy;
begin
  FPersistentClasses.Free;
  inherited;
end;

function TdormConfiguration.GetEnvName(envType: TdormEnvironmentType): string;
begin
  case envType of
    etDevelopment:
      result := 'development';
    etTest:
      result := 'test';
    etRelease:
      result := 'release';
  end;
end;

procedure TdormConfiguration.Configure(envType: TdormEnvironmentType);
var
  s: string;
  i: integer;
  environmentSO: ISuperObject;
  perClasses: TSuperArray;
  mappingReader: TTextReader;
  mappingFile: String;
begin
  environmentSO := FConfigurationSO.O['persistence'].O[GetEnvName(envType)];
  FLogClass := FConfigurationSO.O['config'].s['logger_class_name'];
  perClasses := FConfigurationSO.O['persistence'].A['persistent_classes'];
  FEnvironment.ParseEnvironmentConfiguration(environmentSO, envType,
    GetEnvName(envType));
  for i := 0 to perClasses.Length - 1 do
    PersistentClasses.Add(perClasses[i].AsString);
  // parse the mapping file
  mappingFile := FConfigurationSO.O['config'].s['mapping_file'];
  if mappingFile <> '' then
  begin
    mappingReader := TStreamReader.Create(mappingFile);
    try
      s := mappingReader.ReadToEnd;
    finally
      mappingReader.Free;
    end;
    FMapping := TSuperObject.ParseString(pwidechar(s), true);
    { todo: if mapping is done with attributes, what we should do? }
    if not assigned(FMapping) then
      raise Exception.Create('Cannot parse mapping the file');
  end;
end;

{ TdormEnvironment }
procedure TdormEnvironment.ParseEnvironmentConfiguration(json: ISuperObject;
  envType: TdormEnvironmentType; envName: string);
begin
  Environment := envType;
  EnvironmentName := envName;
  DatabaseAdapter := json.s['database_adapter'];
  DBConnectionString := json.s['database_connection_string'];
  HostName := json.s['hostname'];
  UserName := json.s['username'];
  Password := json.s['password'];
  KeysGenerator := json.s['keys_generator'];
  KeyType := json.s['key_type'];
  if KeyType = 'string' then
    NullKeyValue := json.s['null_key_value']
  else
    NullKeyValue := json.i['null_key_value'];
end;

end.
