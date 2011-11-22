unit dorm.Mappings;

interface

uses
  Rtti;

type
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

end.
