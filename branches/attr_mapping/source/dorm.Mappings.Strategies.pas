unit dorm.Mappings.Strategies;

interface

uses
  Rtti,
  Generics.Collections,
  superobject,
  dorm.Commons,
  dorm.Mappings;

type
  IMappingStrategy = interface
    ['{C580ADCC-B58C-4BDC-AB19-D0E6A0B539D5}']
    function GetTableName(const AType: TRttiType): String;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
  end;

  IMappingStrategyGetter = interface(IMappingStrategy)
    ['{B8C136E4-7C71-4D09-9016-61D2B2271EFE}']
    function GetMapping(const AType: TRttiType): TMappingTable;
  end;

  TCacheMappingStrategy = class(TInterfacedObject, IMappingStrategy, IMappingStrategyGetter)
  private
    FDelegateMapping: IMappingStrategyGetter;
    FMapping: TDictionary<TRttiType, TMappingTable>;
    //FDictMappingCache: TDictionary<string, TArray <TDormFieldMapping>>;
    function GetMapping(const AType: TRttiType): TMappingTable;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
    function GetTableName(const AType: TRttiType): String;
  public
    constructor Create(const aFileMapping: ISuperObject;
      const aCustomMappingStrategy: IMappingStrategy);
    destructor Destroy; override;
  end;

  TDelegateMappingStrategy = class(TInterfacedObject, IMappingStrategy, IMappingStrategyGetter)
  private
    FStrategies: array of IMappingStrategy;
    function GetMapping(const AType: TRttiType): TMappingTable;
    function GetTableName(const AType: TRttiType): string;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
  public
    constructor Create(const aFileMapping: ISuperObject;
      const aCustomMappingStrategy: IMappingStrategy);
  end;

  TFileMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    FMapping: ISuperObject;
    function FieldName(const AProperty: TRttiProperty): string;
    function PKPropertyName(const AType: TRttiType): string;
    function GetTableName(const AType: TRttiType): string;
  public
    constructor Create(Mapping: ISuperObject);
  end;

  TAttributesMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    function GetAttribute<T:TCustomAttribute>(const Obj: TRttiObject): T;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
    function GetTableName(const AType: TRttiType): String;
  end;

  TCoCMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    function FieldName(const AProperty: TRttiProperty): string;
    function PKPropertyName(const AType: TRttiType): string;
    function GetTableName(const AType: TRttiType): string;
  end;

implementation

uses
  SysUtils, StrUtils;

{ TCacheMappingStrategy }

constructor TCacheMappingStrategy.Create(const aFileMapping: ISuperObject;
  const aCustomMappingStrategy: IMappingStrategy);
begin
  inherited Create;
  FMapping := TDictionary<TRttiType, TMappingTable>.Create(128);
  //FDictMappingCache := TDictionary<string, TArray <TDormFieldMapping>>.Create(128);
  FDelegateMapping := TDelegateMappingStrategy.Create(aFileMapping,
    aCustomMappingStrategy);
end;

destructor TCacheMappingStrategy.Destroy;
var
  maptable: TMappingTable;
begin
  for maptable in FMapping.Values do
    maptable.Free;
  //FDictMappingCache.Free;
  FMapping.Free;
  inherited;
end;

function TCacheMappingStrategy.GetMapping(const AType: TRttiType): TMappingTable;
begin
  if not FMapping.TryGetValue(AType, Result) then
  begin
    Result := FDelegateMapping.GetMapping(AType);
    FMapping.Add(AType, Result);
  end;
end;

function TCacheMappingStrategy.GetTableName(const AType: TRttiType): string;
begin
  Result := GetMapping(AType).TableName;
end;

function TCacheMappingStrategy.PKPropertyName(const AType: TRttiType): String;
begin

end;

function TCacheMappingStrategy.FieldName(
  const AProperty: TRttiProperty): String;
begin

end;

{ TDelegateMappingStrategy }

constructor TDelegateMappingStrategy.Create(const aFileMapping: ISuperObject;
  const aCustomMappingStrategy: IMappingStrategy);
begin
  inherited Create;
  // *** tem que ver aqui se vai ficar memory leaks quando esta classe for destruida
  if Assigned(aCustomMappingStrategy) then
  begin
    SetLength(FStrategies, 4);
    FStrategies[0] := aCustomMappingStrategy;
    FStrategies[1] := TFileMappingStrategy.Create(aFileMapping);
    FStrategies[2] := TAttributesMappingStrategy.Create;
    FStrategies[3] := TCoCMappingStrategy.Create;
  end else begin
    SetLength(FStrategies, 3);
    FStrategies[0] := TFileMappingStrategy.Create(aFileMapping);
    FStrategies[1] := TAttributesMappingStrategy.Create;
    FStrategies[2] := TCoCMappingStrategy.Create;
  end;
end;

function TDelegateMappingStrategy.GetMapping(const AType: TRttiType): TMappingTable;
begin
  Result := TMappingTable.Create;
  Result.TableName := GetTableName(AType);
//  Result.Id := GetId(Result, AType);
end;

function TDelegateMappingStrategy.GetTableName(const AType: TRttiType): string;
var
  Strategy: IMappingStrategy;
begin
  for Strategy in FStrategies do
  begin
    Result := Strategy.GetTableName(AType);
    if Result <> '' then
      Exit;
  end;
  // *** generate exception here?????
end;

function TDelegateMappingStrategy.FieldName(
  const AProperty: TRttiProperty): String;
var
  Strategy: IMappingStrategy;
begin
  for Strategy in FStrategies do
  begin
    Result := Strategy.FieldName(AProperty);
    if Result <> '' then
      Exit;
  end;
end;

function TDelegateMappingStrategy.PKPropertyName(
  const AType: TRttiType): String;
var
  Strategy: IMappingStrategy;
begin
  for Strategy in FStrategies do
  begin
    Result := Strategy.PKPropertyName(AType);
    if Result <> '' then
      Exit;
  end;
end;

{ TFileMappingStrategy }

constructor TFileMappingStrategy.Create(Mapping: ISuperObject);
begin
  inherited Create;
  FMapping := Mapping;
end;

function TFileMappingStrategy.FieldName(const AProperty: TRttiProperty): string;
var
  Fields: TSuperArray;
  Item: ISuperObject;
  i: Integer;
begin
  Result := '';
  Fields := FMapping.O[AProperty.Parent.Name].O['fields'].AsArray;
  for i := 0 to Fields.Length - 1 do
  begin
    Item := Fields.O[i];
    if SameText(AProperty.Name, Item.O['name'].AsString) then
      Exit(Item.O['field'].AsString);
  end;
end;

function TFileMappingStrategy.PKPropertyName(const AType: TRttiType): string;
begin
  Result := FMapping.O[AType.Name].O['id'].O['name'].AsString;
end;

function TFileMappingStrategy.GetTableName(const AType: TRttiType): string;
var
  Table: ISuperObject;
begin
  Table := FMapping.O[AType.Name].O['table'];
  if Assigned(Table) then
    Result := Table.AsString
  else
    Result := '';
end;

{ TAttributesMappingStrategy }

function TAttributesMappingStrategy.FieldName(
  const AProperty: TRttiProperty): String;
var
  Attr: Column;
begin
  Attr := GetAttribute<Column>(AProperty);
  Result := Attr.ColumnName;
end;

function TAttributesMappingStrategy.GetAttribute<T>(const Obj: TRttiObject): T;
var
  Attr: TCustomAttribute;
begin
  Result := Nil;
  for Attr in Obj.GetAttributes do
    if Attr.ClassType.InheritsFrom(T) then
      Result := T(Attr);
end;

function TAttributesMappingStrategy.PKPropertyName(
  const AType: TRttiType): String;
var
  LProperty: TRttiProperty;
  Properties: TArray<TRttiProperty>;
  i: Integer;
begin
  Properties := AType.GetProperties;
  for i := 0 to High(Properties) do
  begin
    LProperty := Properties[i];
    if Assigned(GetAttribute<PrimaryKey>(LProperty)) then
      Result := LProperty.Name;
  end;
end;

function TAttributesMappingStrategy.GetTableName(const AType: TRttiType): String;
var
  Attr: Entity;
begin
  Attr := GetAttribute<Entity>(AType);
  if Assigned(Attr) then
    Result := Attr.TableName
  else
    Result := '';
end;

{ TCoCMappingStrategy }

function TCoCMappingStrategy.FieldName(const AProperty: TRttiProperty): string;
begin
  Result := AnsiUpperCase(AProperty.Name);
end;

function TCoCMappingStrategy.PKPropertyName(const AType: TRttiType): string;
begin
  //TODO: We have to define a good convention for primary key
  Result := '';
end;

function TCoCMappingStrategy.GetTableName(const AType: TRttiType): string;
begin
  if AType.Name[1] = 'T' then
    Result := AnsiUpperCase(Copy(AType.Name, 2))
  else
    Result := AnsiUpperCase(AType.Name);
end;

end.
