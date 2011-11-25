unit TestMappings;

interface

uses
  BaseTestCase, dorm.Mappings, Rtti, TestFramework, superobject;

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

  TMappingStrategyBaseTestCase = class(TTestCase)
  protected
    FContext: TRttiContext;
    FPersonType: TRttiType;
    FCarType: TRttiType;
    FFirstNameProperty: TRttiProperty;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TDelegateMappingStrategyTests = class(TMappingStrategyBaseTestCase)
  private
    FDelegate: IMappingStrategy;
    FMapping1, FMapping2: IFakeMappingStrategy;
  protected
    procedure TearDown; override;
    procedure SetUp; override;
  published
    procedure TestTableName;
    procedure TestFieldName;
    procedure TestPKPropertyName;
  end;

  TAttributesMappingStrategyTests = class(TMappingStrategyBaseTestCase)
  private
    FMapping: IMappingStrategy;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTableName;
    procedure TestFieldName;
    procedure TestPKPropertyName;
  end;

  TCoCMappingStrategyTests = class(TMappingStrategyBaseTestCase)
  private
    FMapping: IMappingStrategy;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTableName;
    procedure TestFieldName;
    procedure TestPKPropertyName;
  end;

  TFileMappingStrategyTests = class(TMappingStrategyBaseTestCase)
  private
    FJson: ISuperObject;
    FMapping: IMappingStrategy;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTableName;
    procedure TestFieldName;
    procedure TestPKPropertyName;
  end;

implementation

uses
  dorm.tests.bo, Classes;

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
end;

procedure TDelegateMappingStrategyTests.TearDown;
begin
  inherited;
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

{ TMappingStrategyBaseTestCase }

procedure TMappingStrategyBaseTestCase.SetUp;
begin
  inherited;
  FContext := TRttiContext.Create;
  FPersonType := FContext.GetType(TPerson);
  FFirstNameProperty := FPersonType.GetProperty('FirstName');

  FCarType := FContext.GetType(TCar);
end;

procedure TMappingStrategyBaseTestCase.TearDown;
begin
  inherited;
  FContext.Free;
end;

{ TAttributesMappingStrategyTests }

procedure TAttributesMappingStrategyTests.SetUp;
begin
  inherited;
  FMapping := TAttributesMappingStrategy.Create;
end;

procedure TAttributesMappingStrategyTests.TearDown;
begin
  inherited;
  FMapping := Nil;
end;

procedure TAttributesMappingStrategyTests.TestFieldName;
begin
  CheckEquals('COLUMN_FIRSTNAME', FMapping.FieldName(FFirstNameProperty));
end;

procedure TAttributesMappingStrategyTests.TestPKPropertyName;
begin
  CheckEquals('ID', FMapping.PKPropertyName(FPersonType));
end;

procedure TAttributesMappingStrategyTests.TestTableName;
begin
  CheckEquals('TABLE_PERSON', FMapping.TableName(FPersonType));

  CheckEquals('', FMapping.TableName(FCarType), 'If the class don''t have the [Entity] attribute, table name should be empty');
end;

{ TCoCMappingStrategyTests }

procedure TCoCMappingStrategyTests.SetUp;
begin
  inherited;
  FMapping := TCoCMappingStrategy.Create;
end;

procedure TCoCMappingStrategyTests.TearDown;
begin
  inherited;
  FMapping := Nil;
end;

procedure TCoCMappingStrategyTests.TestFieldName;
begin
  CheckEquals('FIRSTNAME', FMapping.FieldName(FFirstNameProperty));
end;

procedure TCoCMappingStrategyTests.TestPKPropertyName;
begin
  CheckEquals('', FMapping.PKPropertyName(FPersonType));
end;

procedure TCoCMappingStrategyTests.TestTableName;
begin
  CheckEquals('PERSON', FMapping.TableName(FPersonType));
end;

{ TFileMappingStrategyTests }

procedure TFileMappingStrategyTests.SetUp;
var
  Reader: TStreamReader;
  Content: String;
begin
  inherited;
  Reader := TStreamReader.Create('dorm_mapping_tests.conf');
  try
    Content := Reader.ReadToEnd;

    FJson := TSuperObject.ParseString(PChar(Content), True);
  finally
    Reader.Free;
  end;

  FMapping := TFileMappingStrategy.Create(FJson.O['mapping']);
end;

procedure TFileMappingStrategyTests.TearDown;
begin
  inherited;
  FJson := Nil;
  FMapping := Nil;
end;

procedure TFileMappingStrategyTests.TestFieldName;
begin
  CheckEquals('FIRST_NAME', FMapping.FieldName(FFirstNameProperty));
end;

procedure TFileMappingStrategyTests.TestPKPropertyName;
begin
  CheckEquals('ID', FMapping.PKPropertyName(FPersonType));
end;

procedure TFileMappingStrategyTests.TestTableName;
begin
  CheckEquals('PEOPLE', FMapping.TableName(FPersonType));
end;

initialization
RegisterTest(TDelegateMappingStrategyTests.Suite);
RegisterTest(TAttributesMappingStrategyTests.Suite);
RegisterTest(TCoCMappingStrategyTests.Suite);
RegisterTest(TFileMappingStrategyTests.Suite);

end.
