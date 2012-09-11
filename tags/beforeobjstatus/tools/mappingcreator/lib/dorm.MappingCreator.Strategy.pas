unit dorm.MappingCreator.Strategy;

interface

uses
  System.Classes,
  System.SysUtils,
  Generics.Collections,
  dorm.Configuration,
  dorm.Mappings, Data.DBXJSON;

type
  TMappingCreatorStrategy = class
  private
    FUserParameters: TDictionary<string, string>;
    FWarnings: TList<string>;
    FErrors: TList<string>;
    FTablesClassesDictionary: TDictionary<string, string>;
  protected
    FConfiguration: TdormConfiguration;
    // These methods must be ovewritten by descendants
    procedure InitUserParameters; virtual;
    function GetTablesName: TList<string>; virtual; abstract;
    function CreatePKFieldMapping(TableName: String): TJSONObject;
      virtual; abstract;
    function CreateFieldsMapping(TableName: String): TJSONArray;
      virtual; abstract;
    function GetTableMappingByName(const TableName: string): TMappingTable;
      virtual; abstract;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    function InternalExecute(AOutputStream: TStream): Boolean; virtual;
      abstract;
  strict private
    function IsAvailableSpecificTranslation(ATableName: String;
      out AClassName: String): Boolean;
    function GetDefaultDictionary: TDictionary<string, string>;
  protected
    // These methods should not be overwritten by descendands
    function TableNameToClassName(const TableName: String): String;
  public
    procedure SetConfiguration(Configuration: TdormConfiguration);
      virtual; abstract;
    // These methods should not be overwritten by descendands
    class function FieldNameToClassAtribute(const FieldName: String): String;
    function GetUserParameterByName(const Name: String): String;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddParameter(const Name: String; DefaultValue: String = '');
    function GetUserPropertiesNames: TArray<string>;
    procedure SetUserProperty(const PropertyName: string;
      const PropertyValue: string);
    function Execute(AOutputStream: TStream): Boolean; overload;
    function Execute(AOutputStream: TStream;
      ACustomDictionary: TDictionary<string, string>): Boolean; overload;
    function Warnings: TList<string>;
    function Errors: TList<string>;
  end;

  TMappingCreatorStrategyClass = class of TMappingCreatorStrategy;

  TMappingCreatorStrategyRegister = class
  private
    class var MappingCreatorStrategies
      : TDictionary<string, TMappingCreatorStrategyClass>;
  public
    class procedure Register(MappingCreatorStrategyClass
      : TMappingCreatorStrategyClass;
      const ID: string);
    class destructor Destroy;
    class function GetMappingStrategies
      : TDictionary<string, TMappingCreatorStrategyClass>;
    class function CreateStrategy(const StrategyID: String)
      : TMappingCreatorStrategy;
  end;

implementation

uses
  System.StrUtils;

{ TMappingCreatorStrategyRegister }

class function TMappingCreatorStrategyRegister.CreateStrategy(
  const StrategyID: String): TMappingCreatorStrategy;
var
  clazz: TMappingCreatorStrategyClass;
begin
  if MappingCreatorStrategies.TryGetValue(StrategyID, clazz) then
  begin
    Result := clazz.Create;
  end
  else
    raise Exception.CreateFmt('Strategy not found [%s]', [StrategyID]);
end;

class destructor TMappingCreatorStrategyRegister.Destroy;
begin
  FreeAndNil(MappingCreatorStrategies);
end;

class function TMappingCreatorStrategyRegister.GetMappingStrategies
  : TDictionary<string, TMappingCreatorStrategyClass>;
begin
  Result := MappingCreatorStrategies;
end;

class procedure TMappingCreatorStrategyRegister.Register(
  MappingCreatorStrategyClass: TMappingCreatorStrategyClass; const ID: string);
begin
  if not Assigned(MappingCreatorStrategies) then
  begin
    MappingCreatorStrategies :=
      TDictionary<string, TMappingCreatorStrategyClass>.Create;
  end;
  MappingCreatorStrategies.Add(ID, MappingCreatorStrategyClass);
end;

{ TMappingCreatorStrategy }

procedure TMappingCreatorStrategy.AddParameter(const Name: String;
  DefaultValue: String);
begin
  FUserParameters.Add(Name, DefaultValue);
end;

constructor TMappingCreatorStrategy.Create;
begin
  inherited;
  FErrors := TList<string>.Create;
  FWarnings := TList<string>.Create;
  FUserParameters := TDictionary<string, string>.Create;
  InitUserParameters;
end;

destructor TMappingCreatorStrategy.Destroy;
begin
  FUserParameters.Free;
  FErrors.Free;
  FWarnings.Free;
  inherited;
end;

function TMappingCreatorStrategy.Errors: TList<string>;
begin
  Result := FErrors;
end;

function TMappingCreatorStrategy.Execute(AOutputStream: TStream): Boolean;
var
  DefaultDictionary: TDictionary<string, string>;
begin
  DefaultDictionary := GetDefaultDictionary;
  try
    Result := Execute(AOutputStream, DefaultDictionary);
  finally
    DefaultDictionary.Free;
  end;
end;

function TMappingCreatorStrategy.Execute(AOutputStream: TStream;
  ACustomDictionary: TDictionary<string, string>): Boolean;
begin
  FErrors.Clear;
  FWarnings.Clear;
  FTablesClassesDictionary := ACustomDictionary;
  Connect;
  try
    Result := InternalExecute(AOutputStream);
  finally
    Disconnect;
  end;
end;

class function TMappingCreatorStrategy.FieldNameToClassAtribute(
  const FieldName: String): String;
var
  c: char;
  NextUpper: Boolean;
  OutString: String;
  InString: string;
  FirstChar: Boolean;
begin
  InString := FieldName;
  NextUpper := False;
  OutString := '';
  FirstChar := True;
  for c in InString do
  begin
    if CharInSet(c, [' ', '_']) then
    begin
      NextUpper := True;
    end
    else
    begin
      if NextUpper or FirstChar then
        OutString := OutString + UpCase(c)
      else
        OutString := OutString + LowerCase(c);
      NextUpper := False;
      FirstChar := False;
    end;
  end;
  Result := OutString;
end;

function TMappingCreatorStrategy.GetDefaultDictionary
  : TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create(128);
  Result.Add('customers', 'customer');
  Result.Add('peoples', 'person');
  Result.Add('cars', 'car');
  Result.Add('emails', 'email');
  // Others?

end;

function TMappingCreatorStrategy.GetUserParameterByName(
  const Name: String): String;
begin
  if not FUserParameters.TryGetValue(Name, Result) then
    raise Exception.Create('Invalid parameter name');
end;

function TMappingCreatorStrategy.GetUserPropertiesNames: TArray<string>;
begin
  Result := FUserParameters.Keys.ToArray;
end;

procedure TMappingCreatorStrategy.InitUserParameters;
begin
  raise Exception.Create
    ('TMappingCreatorStrategy.InitUserParameters need to be overwritten by descendants');
end;

function TMappingCreatorStrategy.IsAvailableSpecificTranslation(
  ATableName: String; out AClassName: String): Boolean;
begin
  Result := FTablesClassesDictionary.TryGetValue(ATableName, AClassName);
end;

procedure TMappingCreatorStrategy.SetUserProperty(const PropertyName,
  PropertyValue: string);
begin
  if FUserParameters.ContainsKey(PropertyName) then
    FUserParameters[PropertyName] := PropertyValue
  else
    raise Exception.Create('Invalid parameter name');
end;

function TMappingCreatorStrategy.TableNameToClassName(
  const TableName: String): String;
var
  c: char;
  NextUpper: Boolean;
  OutString: String;
  InString: string;
  FirstChar: Boolean;
  endsWith: string;
begin
  if FTablesClassesDictionary.TryGetValue(TableName, OutString) then
    Exit(OutString);

  InString := TableName;
  NextUpper := False;
  OutString := 'T';
  FirstChar := True;
  for c in InString do
  begin
    if CharInSet(c, [' ', '_']) then
    begin
      NextUpper := True;
    end
    else
    begin
      if NextUpper or FirstChar then
        OutString := OutString + UpCase(c)
      else
        OutString := OutString + LowerCase(c);
      NextUpper := False;
      FirstChar := False;
    end;
  end;

  // try to remove the "es" at the end
  endsWith := Copy(OutString, length(OutString) - 2, 3);
  if endsWith = 'hes' then
    OutString := Copy(OutString, 1, length(OutString) - 2);

  // try to remove the "s" at the end
  if OutString[length(OutString)] = 's' then
    OutString := Copy(OutString, 1, length(OutString) - 1);
  Result := OutString;
end;

function TMappingCreatorStrategy.Warnings: TList<string>;
begin
  Result := FWarnings;
end;

end.
