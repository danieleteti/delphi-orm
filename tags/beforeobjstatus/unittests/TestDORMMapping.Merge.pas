unit TestDORMMapping.Merge;

interface

uses
  BaseTestCase, 
  dorm.Mappings.Strategies,
  Rtti,
  TestFramework,
  superobject,
  dorm.Mappings;

type
  (*
  TGetMappingDelegate = reference to procedure(AType: TRttiType; AMappingTable: TMappingTable);

  IFakeMappingStrategy = interface(IMappingStrategy)
    ['{6D760D36-6056-4CB1-AC2A-9F98852C80B9}']
    procedure SetGetMappingDelegate(const Delegate: TGetMappingDelegate);

    property GetMappingDelegate: TGetMappingDelegate write SetGetMappingDelegate;
  end;

  TFakeMappingStrategy = class(TInterfacedObject, IFakeMappingStrategy, IMappingStrategy)
  private
    FGetMappingDelegate: TGetMappingDelegate;
    procedure GetMapping(const AType: TRttiType; AMappingTable: TMappingTable);
    procedure SetGetMappingDelegate(const Delegate: TGetMappingDelegate);
  end;
  *)

  TRelationKind = (rkHasOne, rkHasMany, rkBelongsTo);

  TMappingTableMergerTests = class(TTestCase)
  private
    FMerger: TMappingTableMerger;
    FMapping1: TMappingTable;
    FMapping2: TMappingTable;
    FMapping3: TMappingTable;
    procedure AddFieldToMappingTable(ATable: TMappingTable;
      const AName, AFieldName, AFieldType: string);
    procedure AddRelation(ATable: TMappingTable; ARelationKind: TRelationKind;
      const AName, AParam1, AParam2: string; ALazy: Boolean = False);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMerge_Entity;
    procedure TestMerge_PackageName;
    procedure TestMerge_TestFields;
    procedure TestMerge_Id;
    procedure TestMerge_HasOne;
    procedure TestMerge_HasMany;
    procedure TestMerge_BelongsTo;
  end;

implementation

uses
  Classes;

(*
{ TFakeMappingStrategy }

procedure TFakeMappingStrategy.GetMapping(const AType: TRttiType; AMappingTable: TMappingTable);
begin
  FGetMappingDelegate(AType, AMappingTable);
end;

procedure TFakeMappingStrategy.SetGetMappingDelegate(
  const Delegate: TGetMappingDelegate);
begin
  FGetMappingDelegate := Delegate;
end;
*)

{ TMappingTableMergerTests }

procedure TMappingTableMergerTests.SetUp;
begin
  inherited;
  FMerger := TMappingTableMerger.Create;
  FMapping1 := TMappingTable.Create;
  FMapping2 := TMappingTable.Create;
  FMapping3 := TMappingTable.Create;
end;

procedure TMappingTableMergerTests.TearDown;
begin
  inherited;
  FMapping3.Free;
  FMapping2.Free;
  FMapping1.Free;
  FMerger.Free;
end;

procedure TMappingTableMergerTests.AddFieldToMappingTable(ATable: TMappingTable;
  const AName, AFieldName, AFieldType: string);
begin
  with ATable.AddField do
  begin
    Name := AName;
    FieldName := AFieldName;
    FieldType := AFieldType;
  end;
end;

procedure TMappingTableMergerTests.AddRelation(ATable: TMappingTable;
  ARelationKind: TRelationKind; const AName, AParam1, AParam2: string;
  ALazy: Boolean = False);
begin
  case ARelationKind of
    rkHasOne:
      with ATable.AddHasOne do
      begin
        Name := AName;
        ChildClassName := AParam1;
        ChildFieldName := AParam2;
        LazyLoad := ALazy;
      end;
    rkHasMany:
      with ATable.AddHasMany do
      begin
        Name := AName;
        ChildClassName := AParam1;
        ChildFieldName := AParam2;
        LazyLoad := ALazy;
      end;
    rkBelongsTo:
      with ATable.AddBelongsTo do
      begin
        Name := AName;
        OwnerClassName := AParam1;
        RefFieldName := AParam2;
        LazyLoad := ALazy;
      end;
  end;
end;

procedure TMappingTableMergerTests.TestMerge_Entity;
var
  _Output: TMappingTable;
begin
  _Output := TMappingTable.Create;
  try
    FMapping1.TableName := 'Foo';
    FMerger.Merge(_Output, [FMapping1]);
    CheckEquals('Foo', _Output.TableName, 'Error on merge just Table (Foo)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.TableName := 'Foo';
    FMapping2.TableName := '';
    FMerger.Merge(_Output, [FMapping1, FMapping2]);
    CheckEquals('Foo', _Output.TableName, 'Error on merge Tables (Foo,Empty)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.TableName := '';
    FMapping2.TableName := 'Foo';
    FMerger.Merge(_Output, [FMapping1, FMapping2]);
    CheckEquals('Foo', _Output.TableName, 'Error on merge Tables (Empty,Foo)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.TableName := '';
    FMapping2.TableName := 'Foo';
    FMapping3.TableName := 'Bar';
    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);
    CheckEquals('Foo', _Output.TableName, 'Error on merge Tables (Empty,Foo,Bar)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.TableName := '';
    FMapping2.TableName := 'Foo';
    FMapping3.TableName := '';
    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);
    CheckEquals('Foo', _Output.TableName, 'Error on merge Tables (Empty,Foo,Empty)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.TableName := 'Foo';
    FMapping2.TableName := 'Bar';
    FMapping3.TableName := 'Kro';
    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);
    CheckEquals('Foo', _Output.TableName, 'Error on merge Tables (Foo,Bar,Kro)');
  finally
    _Output.Free;
  end;
end;

procedure TMappingTableMergerTests.TestMerge_PackageName;
var
  _Output: TMappingTable;
begin
  _Output := TMappingTable.Create;
  try
    FMapping1.Package := 'Foo';
    FMerger.Merge(_Output, [FMapping1]);
    CheckEquals('Foo', _Output.Package, 'Error on merge just Package (Foo)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.Package := 'Foo';
    FMapping2.Package := '';
    FMerger.Merge(_Output, [FMapping1, FMapping2]);
    CheckEquals('Foo', _Output.Package, 'Error on merge Packages (Foo,Empty)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.Package := '';
    FMapping2.Package := 'Foo';
    FMerger.Merge(_Output, [FMapping1, FMapping2]);
    CheckEquals('Foo', _Output.Package, 'Error on merge Packages (Empty,Foo)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.Package := '';
    FMapping2.Package := 'Foo';
    FMapping3.Package := 'Bar';
    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);
    CheckEquals('Foo', _Output.Package, 'Error on merge Packages (Empty,Foo,Bar)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.Package := '';
    FMapping2.Package := 'Foo';
    FMapping3.Package := '';
    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);
    CheckEquals('Foo', _Output.Package, 'Error on merge Packages (Empty,Foo,Empty)');
  finally
    _Output.Free;
  end;

  _Output := TMappingTable.Create;
  try
    FMapping1.Package := 'Foo';
    FMapping2.Package := 'Bar';
    FMapping3.Package := 'Kro';
    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);
    CheckEquals('Foo', _Output.Package, 'Error on merge Packages (Foo,Bar,Kro)');
  finally
    _Output.Free;
  end;
end;

procedure TMappingTableMergerTests.TestMerge_TestFields;
var
  _Output: TMappingTable;
  _Field: TMappingField;
begin
  _Output := TMappingTable.Create;
  try
    AddFieldToMappingTable(FMapping1, 'PropertyA', 'FieldA', 'String');
    AddFieldToMappingTable(FMapping1, 'PropertyB', '', 'blob');

    AddFieldToMappingTable(FMapping2, 'PropertyA', 'FieldB', 'Integer');
    AddFieldToMappingTable(FMapping2, 'PropertyB', 'FieldC', 'String');
    AddFieldToMappingTable(FMapping2, 'PropertyC', 'FieldD', 'Integer');

    FMerger.Merge(_Output, [FMapping1, FMapping2]);

    _Field := _Output.FindByName('PropertyA');
    CheckFalse(_Field.IsPK, 'PropertyA dont should be a PK (2m)');
    CheckEquals('FieldA', _Field.FieldName, 'FieldName should be FIELDA (2m)');
    CheckEquals('String', _Field.FieldType);
    CheckEquals('', _Field.DefaultValue);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckTrue(itNone = _Field.IndexType);

    _Field := _Output.FindByName('PropertyB');
    CheckFalse(_Field.IsPK, 'PropertyB dont should be a PK (2m)');
    CheckEquals('FieldC', _Field.FieldName, 'FieldName should be FieldB (2m)');
    CheckEquals('blob', _Field.FieldType);
    CheckEquals('', _Field.DefaultValue);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckTrue(itNone = _Field.IndexType);

    _Field := _Output.FindByName('PropertyC');
    CheckFalse(_Field.IsPK, 'PropertyC dont should be a PK (2m)');
    CheckEquals('FieldD', _Field.FieldName, 'FieldName should be FieldD (2m)');
    CheckEquals('Integer', _Field.FieldType);
    CheckEquals('', _Field.DefaultValue);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckTrue(itNone = _Field.IndexType);

  finally
    _Output.Free;
  end;
end;

procedure TMappingTableMergerTests.TestMerge_Id;
var
  _Output: TMappingTable;
  _Field: TMappingField;
begin
  _Output := TMappingTable.Create;
  try
    FMapping1.TableName := 'Foo';
    _Field := FMapping1.AddField;
    with _Field do
    begin
      Name := 'PropertyA';
      FieldName := 'FieldA';
      FieldType := 'integer';
      IsPK := False;
    end;

    FMerger.Merge(_Output, [FMapping1]);

    _Field := _Output.FindByName('PropertyA');
    CheckNotNull(_Field, 'PropertyA should be exists in Foo');
    CheckFalse(_Field.IsPK, 'PropertyA dont should be a PK here (1m)');
    CheckEquals('FieldA', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals('', _Field.DefaultValue);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckTrue(itNone = _Field.IndexType);

    FMapping2.TableName := 'Bar';
    _Field := FMapping2.AddField;
    with _Field do
    begin
      Name := 'PropertyA';
      IsPK := True;
    end;

    FMerger.Merge(_Output, [FMapping1, FMapping2]);

    _Field := _Output.FindByName('PropertyA');
    CheckNotNull(_Field, 'PropertyA should be exists in Foo');
    CheckTrue(_Field.IsPK, 'PropertyA should be a PK here (2m)');
    CheckEquals('FieldA', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals('', _Field.DefaultValue);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckTrue(itNone = _Field.IndexType);

    FMapping3.TableName := 'FooBar';
    _Field := FMapping3.AddField;
    with _Field do
      Name := 'PropertyA';

    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);

    _Field := _Output.FindByName('PropertyA');
    CheckNotNull(_Field, 'PropertyA should be exists in Foo');
    CheckTrue(_Field.IsPK, 'PropertyA should be a PK here (3m)');
    CheckEquals('FieldA', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals('', _Field.DefaultValue);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckTrue(itNone = _Field.IndexType);

  finally
    _Output.Free;
  end;
end;

procedure TMappingTableMergerTests.TestMerge_HasOne;
var
  _Output: TMappingTable;
  _Relation: TMappingRelation;
begin
  _Output := TMappingTable.Create;
  try
    AddRelation(FMapping1, rkHasOne, 'PropertyA', '', 'Class1.PropertyB');
    AddRelation(FMapping2, rkHasOne, 'PropertyA', 'Class1', 'Class1.PropertyC');
    AddRelation(FMapping3, rkHasOne, 'PropertyA', '', '', True);
    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);
    _Relation := _Output.FindHasOneByName('PropertyA');
    CheckNotNull(_Relation, 'Should be exists a HasOne to PropertyA');
    CheckEquals('Class1', _Relation.ChildClassName, 'PropertyA HasOne.ChildClassName should be Class1');
    CheckEquals('Class1.PropertyB', _Relation.ChildFieldName, 'PropertyA HasOne.ChildClassName should be Class1.PropertyB');
    CheckTrue(_Relation.LazyLoad, 'PropertyA sould be lazy');
  finally
    _Output.Free;
  end;
end;

procedure TMappingTableMergerTests.TestMerge_HasMany;
var
  _Output: TMappingTable;
  _Relation: TMappingRelation;
begin
  _Output := TMappingTable.Create;
  try
    AddRelation(FMapping1, rkHasMany, 'PropertyA', '', 'Class1.PropertyB');
    AddRelation(FMapping2, rkHasMany, 'PropertyA', 'Class1', 'Class1.PropertyC');
    AddRelation(FMapping3, rkHasMany, 'PropertyA', '', '', True);
    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);
    _Relation := _Output.FindHasManyByName('PropertyA');
    CheckNotNull(_Relation, 'Should be exists a HasMany to PropertyA');
    CheckEquals('Class1', _Relation.ChildClassName, 'PropertyA HasMany.ChildClassName should be Class1');
    CheckEquals('Class1.PropertyB', _Relation.ChildFieldName, 'PropertyA HasMany.ChildClassName should be Class1.PropertyB');
    CheckTrue(_Relation.LazyLoad, 'PropertyA sould be lazy');
  finally
    _Output.Free;
  end;
end;

procedure TMappingTableMergerTests.TestMerge_BelongsTo;
var
  _Output: TMappingTable;
  _Relation: TMappingBelongsTo;
begin
  _Output := TMappingTable.Create;
  try
    AddRelation(FMapping1, rkBelongsTo, 'PropertyA', '', 'Class1.PropertyB');
    AddRelation(FMapping2, rkBelongsTo, 'PropertyA', 'Class1', 'Class1.PropertyC');
    AddRelation(FMapping3, rkBelongsTo, 'PropertyA', '', '', True);
    FMerger.Merge(_Output, [FMapping1, FMapping2, FMapping3]);
    _Relation := _Output.FindBelongsToByName('PropertyA');
    CheckNotNull(_Relation, 'Should be exists a BelongsTo to PropertyA');
    CheckEquals('Class1', _Relation.OwnerClassName, 'PropertyA BelongsTo.OwnerClassName should be Class1');
    CheckEquals('Class1.PropertyB', _Relation.RefFieldName, 'PropertyA BelongsTo.RefFieldName should be Class1.PropertyB');
    CheckTrue(_Relation.LazyLoad, 'PropertyA sould be lazy');
  finally
    _Output.Free;
  end;
end;

initialization
  RegisterTest(TMappingTableMergerTests.Suite);

end.
