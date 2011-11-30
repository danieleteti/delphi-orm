unit dorm.Mappings;

interface

uses
  Rtti, 
  Generics.Collections,
  dorm.Commons,
  superobject;

type
  Entity = class(TCustomAttribute)
  private
    FTableName: String;
  public
    constructor Create(const TableName: String = '');
    property TableName: String read FTableName;
  end;

  Column = class(TCustomAttribute)
  private
    FColumnName: String;
  public
    constructor Create(const ColumnName: String);
    property ColumnName: String read FColumnName;
  end;

  PrimaryKey = class(TCustomAttribute);

  IMappingStrategy = interface
    ['{C580ADCC-B58C-4BDC-AB19-D0E6A0B539D5}']
    function TableName(const AType: TRttiType): String;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
  end;

  TCacheMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    FDelegateMapping: IMappingStrategy;
    FTableNamesCache: TDictionary<TRttiType, String>;
    FDictMappingCache: TDictionary<string, TArray <TDormFieldMapping>>;
  public
    constructor Create(const aFileMapping: ISuperObject;
      const aCustomMappingStrategy: IMappingStrategy);
    destructor Destroy; override;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
    function TableName(const AType: TRttiType): String;
  end;

  TDelegateMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    FStrategies: array of IMappingStrategy;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
    function TableName(const AType: TRttiType): String;
  public
    constructor Create(const aFileMapping: ISuperObject;
      const aCustomMappingStrategy: IMappingStrategy);
  end;

  TFileMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    FMapping: ISuperObject;
    function FieldName(const AProperty: TRttiProperty): string;
    function PKPropertyName(const AType: TRttiType): string;
    function TableName(const AType: TRttiType): string;
  public
    constructor Create(Mapping: ISuperObject);
  end;

  TAttributesMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    function GetAttribute<T:TCustomAttribute>(const Obj: TRttiObject): T;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
    function TableName(const AType: TRttiType): String;
  end;

  TCoCMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    function FieldName(const AProperty: TRttiProperty): string;
    function PKPropertyName(const AType: TRttiType): string;
    function TableName(const AType: TRttiType): string;
  end;

implementation

uses
  SysUtils, StrUtils;

{ Entity }

constructor Entity.Create(const TableName: String);
begin
  FTableName := TableName;
end;

{ Column }

constructor Column.Create(const ColumnName: String);
begin
  FColumnName := ColumnName;
end;

{ TCacheMappingStrategy }

constructor TCacheMappingStrategy.Create(const aFileMapping: ISuperObject;
  const aCustomMappingStrategy: IMappingStrategy);
begin
  inherited Create;
  FTableNamesCache := TDictionary<TRttiType, String>.Create(128);
  FDictMappingCache := TDictionary<string, TArray <TDormFieldMapping>>.Create(128);
  FDelegateMapping := TDelegateMappingStrategy.Create(
    aFileMapping, aCustomMappingStrategy);
end;

destructor TCacheMappingStrategy.Destroy;
begin
  FDictMappingCache.Free;
  FTableNamesCache.Free;
  inherited;
end;

function TCacheMappingStrategy.FieldName(
  const AProperty: TRttiProperty): String;
begin

end;

function TCacheMappingStrategy.PKPropertyName(const AType: TRttiType): String;
begin

end;

function TCacheMappingStrategy.TableName(const AType: TRttiType): String;
begin
  if not FTableNamesCache.TryGetValue(AType, Result) then
  begin
    Result := FDelegateMapping.TableName(AType);
    FTablenamesCache.Add(AType, Result);
  end;
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

function TDelegateMappingStrategy.TableName(const AType: TRttiType): String;
var
  Strategy: IMappingStrategy;
begin
  for Strategy in FStrategies do
  begin
    Result := Strategy.TableName(AType);
    if Result <> '' then
      Exit;
  end;
end;

{ TFileMappingStrategy }

constructor TFileMappingStrategy.Create(Mapping: ISuperObject);
begin
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

function TFileMappingStrategy.TableName(const AType: TRttiType): string;
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

function TAttributesMappingStrategy.TableName(const AType: TRttiType): String;
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

function TCoCMappingStrategy.TableName(const AType: TRttiType): string;
begin
  if AType.Name[1] = 'T' then
    Result := AnsiUpperCase(Copy(AType.Name, 2))
  else
    Result := AnsiUpperCase(AType.Name);
end;

end.
