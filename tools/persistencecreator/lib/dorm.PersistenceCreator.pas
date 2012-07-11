unit dorm.PersistenceCreator;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Contnrs,
  dorm,
  dorm.Commons;

type
  TPersistenceEnvironment = class;
  TPersistenceClass = class;
  TPersistenceConfig = class;

  TPersistenceCreator = class(TObject)
  strict private
  private
    procedure SetDefaultProperty(var Env: TPersistenceEnvironment);
  protected
    FConfig: TPersistenceConfig;
    FPersistenceClasses: TObjectList<TPersistenceClass>;
    FPersistenceClassesList: TStrings;
    FEnvironments: TObjectList<TPersistenceEnvironment>;
    FEnvironmentsList: TStrings;
    procedure InitEnvironments; virtual;
    procedure InitPersistenceClasses; virtual;
    procedure InitConfig; virtual;
    procedure LoadEnvironments; virtual;
    procedure LoadPersistenceClasses; virtual;
    procedure LoadConfig; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure Load(APersistenceFileName: string); virtual;
    procedure Save(APersistenceFileName: string); virtual;

    procedure SetConfig(const Value: TPersistenceConfig);
    procedure SetEnvironments(const Value: TObjectList<TPersistenceEnvironment>);
    procedure SetPersistenceClasses(const Value: TObjectList<TPersistenceClass>);
    procedure SetPersistenceClassesList(const Value: TStrings);

    function GetConfig: TPersistenceConfig;
    function GetEnvironments: TObjectList<TPersistenceEnvironment>;
    function GetPersistenceClasses: TObjectList<TPersistenceClass>;
    function GetEnvironmentsList: TStrings;
    function GetPersistenceClassesList: TStrings;
  end;

  TPersistenceEnvironment = class(TObject)
  strict private
    FEnvironmentName: string;
    FDatabaseConnectionString: string;
    FNullKeyValue: string;
    FDatabaseAdapter: string;
    FKeyType: string;
    FKeysGenerator: string;
    FCustomAdapterParameter: TStrings;
    procedure SetDatabaseAdapter(const Value: string);
    procedure SetDatabaseConnectionString(const Value: string);
    procedure SetKeysGenerator(const Value: string);
    procedure SetKeyType(const Value: string);
    procedure SetNullKeyValue(const Value: string);
    procedure SetCustomAdapterParameter(const Value: TStrings);
  protected
  public
    constructor Create(AEnvironmentName: string);
    destructor Destroy; override;
    property EnvironmentName: string read FEnvironmentName;
    property DatabaseAdapter: string read FDatabaseAdapter write SetDatabaseAdapter;
    property DatabaseConnectionString: string read FDatabaseConnectionString write SetDatabaseConnectionString;
    property KeysGenerator: string read FKeysGenerator write SetKeysGenerator;
    property KeyType: string read FKeyType write SetKeyType;
    property NullKeyValue: string read FNullKeyValue write SetNullKeyValue;
    property CustomAdapterParameter: TStrings read FCustomAdapterParameter write SetCustomAdapterParameter;
  end;

  TPersistenceClass = class(TObject)
  strict private
    FPersistenceClassName: string;
  protected
  public
    property PersistenceClassName: string read FPersistenceClassName;
    constructor Create(APersistenceClassName: string);
  end;

  TPersistenceConfig = class(TObject)
  strict private
    FLoggerClassName: string;
    procedure SetLoggerClassName(const Value: string);
  protected
  public
    property LoggerClassName: string read FLoggerClassName write SetLoggerClassName;
  end;

implementation

{ TPersistenceConfig }

procedure TPersistenceConfig.SetLoggerClassName(const Value: string);
begin
  FLoggerClassName := Value;
end;

{ TPersistenceCreator }

constructor TPersistenceCreator.Create;
begin
  FEnvironments := TObjectList<TPersistenceEnvironment>.Create;
  FEnvironments.OwnsObjects := True;
  FPersistenceClasses := TObjectList<TPersistenceClass>.Create;
  FPersistenceClasses.OwnsObjects := True;

  FPersistenceClassesList := TStringList.Create;
  FEnvironmentsList := TStringList.Create;
end;

destructor TPersistenceCreator.Destroy;
begin
  if Assigned(FEnvironments) then FEnvironments.Free;
  if Assigned(FPersistenceClasses) then FPersistenceClasses.Free;
  if Assigned(FPersistenceClassesList) then FPersistenceClassesList.Free;
  if Assigned(FEnvironmentsList) then FEnvironmentsList.Free;
  inherited;
end;

function TPersistenceCreator.GetConfig: TPersistenceConfig;
begin
  Result := FConfig;
end;

function TPersistenceCreator.GetEnvironments: TObjectList<TPersistenceEnvironment>;
begin
  Result := FEnvironments;
end;

function TPersistenceCreator.GetEnvironmentsList: TStrings;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(FEnvironments) and (FEnvironments.Count>0) then
  begin
    FEnvironmentsList.Clear;
    for i := 0 to FEnvironments.Count - 1 do
    begin
      FEnvironmentsList.Add(FEnvironments[i].EnvironmentName);
    end;
    Result := FEnvironmentsList;
  end;
end;

function TPersistenceCreator.GetPersistenceClasses: TObjectList<TPersistenceClass>;
begin
  Result := FPersistenceClasses;
end;

function TPersistenceCreator.GetPersistenceClassesList: TStrings;
var
  i: Integer;
begin
  if Assigned(FPersistenceClasses) and (FPersistenceClasses.Count>0) then
  begin
    FPersistenceClassesList.Clear;
    for i := 0 to FPersistenceClasses.Count - 1 do
    begin
      FPersistenceClassesList.Add(FPersistenceClasses[i].PersistenceClassName);
    end;
  end
  else
  begin
    FPersistenceClassesList.Clear;
  end;
  Result := FPersistenceClassesList;
end;

procedure TPersistenceCreator.InitConfig;
var
  Cfg: TPersistenceConfig;
begin
  inherited;
  Cfg := TPersistenceConfig.Create;
  Cfg.LoggerClassName := 'dorm.loggers.CodeSite.TdormFileLog';
  FConfig := Cfg;
end;

procedure TPersistenceCreator.InitEnvironments;
var
  Env: TPersistenceEnvironment;
begin
  inherited;
  FEnvironments.Clear;
  Env := TPersistenceEnvironment.Create('development');
  SetDefaultProperty(Env);
  FEnvironments.Add(Env);
  Env := TPersistenceEnvironment.Create('test');
  SetDefaultProperty(Env);
  FEnvironments.Add(Env);
  Env := TPersistenceEnvironment.Create('release');
  SetDefaultProperty(Env);
  FEnvironments.Add(Env);
end;

procedure TPersistenceCreator.Initialize;
begin
  InitEnvironments;
  InitPersistenceClasses;
  InitConfig;
end;

procedure TPersistenceCreator.SetDefaultProperty(var Env: TPersistenceEnvironment);
begin
  Env.DatabaseAdapter := 'Select DatabaseAdapter';
  Env.DatabaseConnectionString := 'Set DatabaseConnectionString';
  Env.KeysGenerator := 'Select KeysGenerator';
  Env.KeyType := 'Set KeyType';
  Env.NullKeyValue := 'Set NullKeyValue';
end;

procedure TPersistenceCreator.InitPersistenceClasses;
begin
  inherited;
  FPersistenceClasses.Clear;
  FPersistenceClasses.Add(TPersistenceClass.Create('TPerson'));
  FPersistenceClasses.Add(TPersistenceClass.Create('TPhone'));
  FPersistenceClasses.Add(TPersistenceClass.Create('TCar'));
  FPersistenceClasses.Add(TPersistenceClass.Create('TEmail'));
end;

procedure TPersistenceCreator.Load(APersistenceFileName: string);
begin

end;

procedure TPersistenceCreator.LoadConfig;
begin

end;

procedure TPersistenceCreator.LoadEnvironments;
begin

end;

procedure TPersistenceCreator.LoadPersistenceClasses;
begin

end;

procedure TPersistenceCreator.Save(APersistenceFileName: string);
begin

end;

procedure TPersistenceCreator.SetConfig(const Value: TPersistenceConfig);
begin
  FConfig := Value;
end;

procedure TPersistenceCreator.SetEnvironments(
  const Value: TObjectList<TPersistenceEnvironment>);
begin
  FEnvironments := Value;
end;

procedure TPersistenceCreator.SeTPersistenceClasses(
  const Value: TObjectList<TPersistenceClass>);
begin
  FPersistenceClasses := Value;
end;

procedure TPersistenceCreator.SetPersistenceClassesList(const Value: TStrings);
var
  i: Integer;
  PersClasses: TPersistenceClass;
begin
  if Assigned(Value) and (Value.Count>0) then
  begin
    FPersistenceClasses.Clear;

    for i := 0 to Value.Count - 1 do
    begin
      PersClasses := TPersistenceClass.Create(Value.Strings[i]);
      FPersistenceClasses.Add(PersClasses);
    end;
  end;
end;

{ TPersistenceEnvironment }

constructor TPersistenceEnvironment.Create(AEnvironmentName: string);
begin
  FEnvironmentName := AEnvironmentName;
  FCustomAdapterParameter := TStringList.Create;
end;

destructor TPersistenceEnvironment.Destroy;
begin
  FCustomAdapterParameter.Free;
  inherited;
end;

procedure TPersistenceEnvironment.SetCustomAdapterParameter(
  const Value: TStrings);
begin
  FCustomAdapterParameter := Value;
end;

procedure TPersistenceEnvironment.SetDatabaseAdapter(
  const Value: string);
begin
  FDatabaseAdapter := Value;
end;

procedure TPersistenceEnvironment.SetDatabaseConnectionString(
  const Value: string);
begin
  FDatabaseConnectionString := Value;
end;

procedure TPersistenceEnvironment.SetKeysGenerator(
  const Value: string);
begin
  FKeysGenerator := Value;
end;

procedure TPersistenceEnvironment.SetKeyType(const Value: string);
begin
  FKeyType := Value;
end;

procedure TPersistenceEnvironment.SetNullKeyValue(const Value: string);
begin
  FNullKeyValue := Value;
end;

{ TPersistenceClass }

constructor TPersistenceClass.Create(APersistenceClassName: string);
begin
  FPersistenceClassName := APersistenceClassName;
end;

end.
