unit dorm.Mappings;

interface

uses
  Rtti;

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

  IMappingStrategy = interface
    ['{C580ADCC-B58C-4BDC-AB19-D0E6A0B539D5}']
    function TableName(const AType: TRttiType): String;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
  end;

  TDelegateMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    FStrategies: array of IMappingStrategy;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
    function TableName(const AType: TRttiType): String;
  public
    constructor Create(const Strategies: array of IMappingStrategy);
  end;

  TAttributesMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    function GetAttribute<T:TCustomAttribute>(const Obj: TRttiObject): T;
    function FieldName(const AProperty: TRttiProperty): String;
    function PKPropertyName(const AType: TRttiType): String;
    function TableName(const AType: TRttiType): String;
  end;

implementation

{ TDelegateMappingStrategy }

constructor TDelegateMappingStrategy.Create(
  const Strategies: array of IMappingStrategy);
var
  i: Integer;
begin
  SetLength(FStrategies, Length(Strategies));
  for i := 0 to High(Strategies) do
    FStrategies[i] := Strategies[i];
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

{ Entity }

constructor Entity.Create(const TableName: String);
begin
  FTableName := TableName;
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
    if Attr is T then
      Result := T(Attr);
end;

function TAttributesMappingStrategy.PKPropertyName(
  const AType: TRttiType): String;
begin

end;

function TAttributesMappingStrategy.TableName(const AType: TRttiType): String;
var
  Attr: Entity;
begin
  Attr := GetAttribute<Entity>(AType);
  Result := Attr.TableName;
end;

{ Column }

constructor Column.Create(const ColumnName: String);
begin
  FColumnName := ColumnName;
end;

end.
