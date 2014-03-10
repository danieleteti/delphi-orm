unit dorm.PersistenceCreator.PersistenceJsonFile;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  SuperObject,
  Data.DBXJSON,
  Rtti,
  dorm,
  dorm.Commons,
  dorm.PersistenceCreator,
  Variants;

type
  TPersistenceJsonFileCreator = class(TPersistenceCreator)
  strict private
    FPersistenceJsonFileName: string;
    FPersistenceJsonFile: ISuperObject;
    function PersistenceJSONObjectExist(ASection: string;
      APersistenceJSONObjectName: string = ''): Boolean;
    procedure SetProperty(AEnv: TPersistenceEnvironment; AEnvironments: string);
  protected
    procedure LoadEnvironments; override;
    procedure LoadPersistenceClasses; override;
    procedure LoadConfig; override;
  public
    procedure Load(APersistenceFileName: string); override;
    procedure Save(APersistenceFileName: string); override;
    procedure InitializeFile(APersistenceFileName: String); override;
  end;

implementation

{ TPersistenceJsonFileCreator }

procedure TPersistenceJsonFileCreator.InitializeFile
  (APersistenceFileName: String);
var
  JSON: ISuperObject;
  i: Integer;
  x: Integer;
  PersistentClasses: ISuperObject;
begin
  inherited;
  JSON := TSuperObject.Create(StObject);
  JSON['persistence'] := SO();
  for i := 0 to FEnvironments.Count - 1 do
  begin
    JSON['persistence'].O[FEnvironments[i].EnvironmentName] := SO();
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S
      ['database_adapter'] := FEnvironments[i].DatabaseAdapter;
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S
      ['database_connection_string'] := FEnvironments[i]
      .DatabaseConnectionString;
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S['keys_generator']
      := FEnvironments[i].KeysGenerator;
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S['key_type'] :=
      FEnvironments[i].KeyType;
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S['null_key_value']
      := FEnvironments[i].NullKeyValue;
    for x := 0 to FEnvironments[i].CustomAdapterParameter.Count - 1 do
    begin
      JSON['persistence'].O[FEnvironments[i].EnvironmentName].S
        [FEnvironments[i].CustomAdapterParameter.Names[x]] :=
        FEnvironments[i].CustomAdapterParameter.ValueFromIndex[x];
    end;
  end;
  JSON['persistence'].O['persistent_classes'] := SA([]);
  PersistentClasses := SA([]);
  for i := 0 to FPersistenceClasses.Count - 1 do
  begin
    PersistentClasses.AsArray.S[i] := FPersistenceClasses[i]
      .PersistenceClassName;
  end;
  JSON['persistence'].O['persistent_classes'] := PersistentClasses;
  JSON['config'] := SO();
  JSON.O['config'].S['logger_class_name'] := FConfig.LoggerClassName;
  JSON.SaveTo(APersistenceFileName, True);
end;

procedure TPersistenceJsonFileCreator.Load(APersistenceFileName: string);
var
  persistenceReader: TStreamReader;
  S: string;
begin
  inherited;
  FPersistenceJsonFileName := APersistenceFileName;
  if FPersistenceJsonFileName = '' then
    raise Exception.Create('Persistence file not found');
  persistenceReader := TStreamReader.Create(FPersistenceJsonFileName);
  try
    S := persistenceReader.ReadToEnd;
  finally
    persistenceReader.Free;
  end;
  FPersistenceJsonFile := TSuperObject.ParseString(pwidechar(S), True);
  PersistenceJSONObjectExist('persistence', 'development');
  PersistenceJSONObjectExist('persistence', 'test');
  PersistenceJSONObjectExist('persistence', 'release');
  PersistenceJSONObjectExist('persistence', 'persistent_classes');
  PersistenceJSONObjectExist('config');
  LoadEnvironments;
  LoadPersistenceClasses;
  LoadConfig;
end;

procedure TPersistenceJsonFileCreator.LoadEnvironments;
var
  Env: TPersistenceEnvironment;
begin
  inherited;
  FEnvironments.Clear;
  Env := TPersistenceEnvironment.Create('development');
  SetProperty(Env, Env.EnvironmentName);
  FEnvironments.Add(Env);
  Env := TPersistenceEnvironment.Create('test');
  SetProperty(Env, Env.EnvironmentName);
  FEnvironments.Add(Env);
  Env := TPersistenceEnvironment.Create('release');
  SetProperty(Env, Env.EnvironmentName);
  FEnvironments.Add(Env);
end;

procedure TPersistenceJsonFileCreator.LoadPersistenceClasses;
var
  PersClasses: TPersistenceClass;
  ArrayClasses: TSuperArray;
  i: Integer;
begin
  inherited;
  FPersistenceClasses.Clear;
  ArrayClasses := FPersistenceJsonFile.O['persistence'].A['persistent_classes'];
  if ArrayClasses.Length = 0 then
    Exception.Create('persistent_classes non present or not valid');
  for i := 0 to ArrayClasses.Length - 1 do
  begin
    PersClasses := TPersistenceClass.Create(ArrayClasses.S[i]);
    FPersistenceClasses.Add(PersClasses);
  end;
end;

function TPersistenceJsonFileCreator.PersistenceJSONObjectExist(ASection,
  APersistenceJSONObjectName: string): Boolean;
begin
  Result := True;
  if APersistenceJSONObjectName <> '' then
  begin
    if FPersistenceJsonFile.O[ASection].O[APersistenceJSONObjectName] = nil then
    begin
      raise Exception.Create
        ('Cannot parse persistence file. Missing persistence JSON object ' +
        APersistenceJSONObjectName);
    end;
  end
  else
  begin
    if FPersistenceJsonFile.O[ASection] = nil then
    begin
      raise Exception.Create
        ('Cannot parse persistence file. Missing persistence JSON section ' +
        APersistenceJSONObjectName);
    end;
  end;
end;

procedure TPersistenceJsonFileCreator.Save(APersistenceFileName: string);
var
  JSON: ISuperObject;
  i: Integer;
  x: Integer;
  PersistentClasses: ISuperObject;
begin
  inherited;
  JSON := TSuperObject.Create(StObject);
  JSON['persistence'] := SO();
  for i := 0 to FEnvironments.Count - 1 do
  begin
    JSON['persistence'].O[FEnvironments[i].EnvironmentName] := SO();
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S
      ['database_adapter'] := FEnvironments[i].DatabaseAdapter;
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S
      ['database_connection_string'] := FEnvironments[i]
      .DatabaseConnectionString;
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S['keys_generator']
      := FEnvironments[i].KeysGenerator;
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S['key_type'] :=
      FEnvironments[i].KeyType;
    JSON['persistence'].O[FEnvironments[i].EnvironmentName].S['null_key_value']
      := FEnvironments[i].NullKeyValue;
    for x := 0 to FEnvironments[i].CustomAdapterParameter.Count - 1 do
    begin
      JSON['persistence'].O[FEnvironments[i].EnvironmentName].S
        [FEnvironments[i].CustomAdapterParameter.Names[x]] :=
        FEnvironments[i].CustomAdapterParameter.ValueFromIndex[x];
    end;
  end;
  JSON['persistence'].O['persistent_classes'] := SA([]);
  PersistentClasses := SA([]);
  for i := 0 to FPersistenceClasses.Count - 1 do
  begin
    PersistentClasses.AsArray.S[i] := FPersistenceClasses[i]
      .PersistenceClassName;
  end;
  JSON['persistence'].O['persistent_classes'] := PersistentClasses;
  JSON['config'] := SO();
  JSON.O['config'].S['logger_class_name'] := FConfig.LoggerClassName;
  JSON.SaveTo(APersistenceFileName, True);
end;

procedure TPersistenceJsonFileCreator.SetProperty(AEnv: TPersistenceEnvironment;
  AEnvironments: string);
var
  Obj: ISuperObject;
  Obj1: ISuperObject;
  ArrNames: TSuperArray;
  ArrValues: TSuperArray;
  i: Integer;
begin
  AEnv.DatabaseAdapter := FPersistenceJsonFile.O['persistence'].O[AEnvironments]
    .S['database_adapter'];
  AEnv.DatabaseConnectionString := FPersistenceJsonFile.O['persistence'].O
    [AEnvironments].S['database_connection_string'];
  AEnv.KeysGenerator := FPersistenceJsonFile.O['persistence'].O[AEnvironments].S
    ['keys_generator'];
  AEnv.KeyType := FPersistenceJsonFile.O['persistence'].O[AEnvironments].S
    ['key_type'];
  AEnv.NullKeyValue := FPersistenceJsonFile.O['persistence'].O[AEnvironments].S
    ['null_key_value'];
  Obj := FPersistenceJsonFile.O['persistence'].O[AEnvironments]
    .AsObject.GetNames;
  Obj1 := FPersistenceJsonFile.O['persistence'].O[AEnvironments]
    .AsObject.GetValues;
  ArrNames := Obj.AsArray;
  ArrValues := Obj1.AsArray;
  for i := 0 to ArrNames.Length - 1 do
  begin
    if (ArrNames.S[i] <> 'database_adapter') and
      (ArrNames.S[i] <> 'database_connection_string') and
      (ArrNames.S[i] <> 'keys_generator') and (ArrNames.S[i] <> 'key_type') and
      (ArrNames.S[i] <> 'null_key_value') then
    begin
      AEnv.CustomAdapterParameter.Add(ArrNames.S[i] + '=' + ArrValues.S[i]);
    end;
  end;
end;

procedure TPersistenceJsonFileCreator.LoadConfig;
var
  Cfg: TPersistenceConfig;
begin
  inherited;
  Cfg := TPersistenceConfig.Create;
  Cfg.LoggerClassName := FPersistenceJsonFile.O['config'].S
    ['logger_class_name'];
  FConfig := Cfg;
end;

end.
