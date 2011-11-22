unit TestMappings;

interface

uses
  BaseTestCase, dorm.Mappings, Rtti, TestFramework;

type
  TTableNameDelegate = reference to function(AType: TRttiType): String;
  TFieldNameDelegate = reference to function(AProperty: TRttiProperty): String;
  TPKPropertyNameDelegate = reference to function(AType: TRttiType): String;

  IFakeMappingStrategy = interface(IMappingStrategy)
    ['{6D760D36-6056-4CB1-AC2A-9F98852C80B9}']
    procedure SetTableNameDelegate(const Value: TTableNameDelegate);
    procedure SetFieldNameDelegate(const Value: TFieldNameDelegate);
    procedure SetPKPropertyNameDelegate(const Value: TPKPropertyNameDelegate);

    property TableNameDelegate: TTableNameDelegate write SetTableNameDelegate;
    property FieldNameDelegate: TFieldNameDelegate write SetFieldNameDelegate;
    property PKPropertyNameDelegate: TPKPropertyNameDelegate write SetPKPropertyNameDelegate;
  end;

  TFakeMappingStrategy = class(TInterfacedObject, IFakeMappingStrategy, IMappingStrategy)
  private
    FTableNameDelegate: TTableNameDelegate;
    FFieldNameDelegate: TFieldNameDelegate;
    FPKPropertyNameDelegate: TPKPropertyNameDelegate;

    function FieldName(const AProperty: TRttiProperty): string;
    function PKPropertyName(const AType: TRttiType): string;
    function TableName(const AType: TRttiType): string;
    procedure SetFieldNameDelegate(const Value: TFieldNameDelegate);
    procedure SetPKPropertyNameDelegate(const Value: TPKPropertyNameDelegate);
    procedure SetTableNameDelegate(const Value: TTableNameDelegate);
  end;

  TDelegateMappingStrategyTests = class(TTestCase)
  private
    FDelegate: IMappingStrategy;
    FMapping1, FMapping2: IFakeMappingStrategy;
    FContext: TRttiContext;
    FPersonType: TRttiType;
    FFirstNameProperty: TRttiProperty;
  protected
    procedure TearDown; override;
    procedure SetUp; override;
  published
    procedure TestTableName;
    procedure TestFieldName;
    procedure TestPKPropertyName;
  end;

implementation

uses
  dorm.tests.bo;

{ TFakeMappingStrategy }

function TFakeMappingStrategy.FieldName(const AProperty: TRttiProperty): string;
begin
  Result := FFieldNameDelegate(AProperty);
end;

function TFakeMappingStrategy.PKPropertyName(const AType: TRttiType): string;
begin
  Result := FPKPropertyNameDelegate(AType);
end;

procedure TFakeMappingStrategy.SetFieldNameDelegate(
  const Value: TFieldNameDelegate);
begin
  FFieldNameDelegate := Value;
end;

procedure TFakeMappingStrategy.SetPKPropertyNameDelegate(
  const Value: TPKPropertyNameDelegate);
begin
  FPKPropertyNameDelegate := Value;
end;

procedure TFakeMappingStrategy.SetTableNameDelegate(
  const Value: TTableNameDelegate);
begin
  FTableNameDelegate := Value;
end;

function TFakeMappingStrategy.TableName(const AType: TRttiType): string;
begin
  Result := FTableNameDelegate(AType);
end;

{ TDelegateMappingStrategyTests }

procedure TDelegateMappingStrategyTests.SetUp;
begin
  inherited;
  FMapping1 := TFakeMappingStrategy.Create;
  FMapping2 := TFakeMappingStrategy.Create;
  FDelegate := TDelegateMappingStrategy.Create([FMapping1, FMapping2]);
  FContext := TRttiContext.Create;
  FPersonType := FContext.GetType(TPerson);
  FFirstNameProperty := FPersonType.GetProperty('FirstName');
end;

procedure TDelegateMappingStrategyTests.TearDown;
begin
  inherited;
  FContext.Free;
  FDelegate := Nil;
  FMapping2 := Nil;
  FMapping1 := Nil;
end;

procedure TDelegateMappingStrategyTests.TestFieldName;
begin
  FMapping1.FieldNameDelegate := function(AProperty: TRttiProperty): String
  begin
    CheckTrue(FFirstNameProperty.Equals(AProperty));
    Result := '';
  end;

  FMapping2.FieldNameDelegate := function(AProperty: TRttiProperty): String
  begin
    CheckTrue(FFirstNameProperty.Equals(AProperty));
    Result := 'A';
  end;

  CheckEquals('A', FDelegate.FieldName(FFirstNameProperty));

  FMapping1.FieldNameDelegate := function(AProperty: TRttiProperty): String
  begin
    CheckTrue(FFirstNameProperty.Equals(AProperty));
    Result := 'B';
  end;

  CheckEquals('B', FDelegate.FieldName(FFirstNameProperty));
end;

procedure TDelegateMappingStrategyTests.TestPKPropertyName;
begin
  FMapping1.PKPropertyNameDelegate := function(AType: TRttiType): String
  begin
    CheckTrue(FPersonType.Equals(AType));
    Result := '';
  end;

  FMapping2.PKPropertyNameDelegate := function(AType: TRttiType): String
  begin
    CheckTrue(FPersonType.Equals(AType));
    Result := 'A';
  end;

  CheckEquals('A', FDelegate.PKPropertyName(FPersonType));

  FMapping1.PKPropertyNameDelegate := function(AType: TRttiType): String
  begin
    CheckTrue(FPersonType.Equals(AType));
    Result := 'B';
  end;

  CheckEquals('B', FDelegate.PKPropertyName(FPersonType));
end;

procedure TDelegateMappingStrategyTests.TestTableName;
begin
  FMapping1.TableNameDelegate := function(AType: TRttiType): String
  begin
    CheckTrue(FPersonType.Equals(AType));
    Result := '';
  end;

  FMapping2.TableNameDelegate := function(AType: TRttiType): String
  begin
    CheckTrue(FPersonType.Equals(AType));
    Result := 'A';
  end;

  CheckEquals('A', FDelegate.TableName(FPersonType));

  FMapping1.TableNameDelegate := function(AType: TRttiType): String
  begin
    CheckTrue(FPersonType.Equals(AType));
    Result := 'B';
  end;

  CheckEquals('B', FDelegate.TableName(FPersonType));
end;

initialization
RegisterTest(TDelegateMappingStrategyTests.Suite);

end.
