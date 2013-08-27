unit dorm.Mappings.Strategies;

interface

uses
  Classes,
  Rtti,
  Generics.Collections,
  superobject,
  dorm.Mappings;

type

  {$RTTI EXPLICIT
  FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])
  METHODS([vcPrivate, vcProtected, vcPublic, vcPublished])
  PROPERTIES([vcPrivate, vcProtected, vcPublic, vcPublished])}

  IMappingStrategy = interface
    ['{F64D6AF3-C4C2-4098-A241-B3401CE3FB03}']
    procedure GetMapping(const AType: TRttiType; ATable: TMappingTable);
  end;

  ICacheMappingStrategy = interface
    ['{B8C136E4-7C71-4D09-9016-61D2B2271EFE}']
    function GetMapping(const AType: TRttiType): TMappingTable;
    procedure Add(const AMappingStrategy: IMappingStrategy);
  end;

  TMappingTableMerger = class
  private
    function MergeFieldTo(AField: TMappingField; AOutput, AInput: TMappingTable): Boolean;
    function MergeRelation(AInRelation, AOutRelation: TMappingRelation): Boolean;
    function MergeHasOneTo(ARelation: TMappingRelation; AOutput: TMappingTable): Boolean;
    function MergeHasManyTo(ARelation: TMappingRelation; AOutput: TMappingTable): Boolean;
    function MergeBelongsTo(ARelation: TMappingBelongsTo; AOutput: TMappingTable): Boolean;

  public
    procedure Merge(AOutput: TMappingTable; AInput: array of TMappingTable);
  end;

  TCacheMappingStrategy = class(TInterfacedObject, ICacheMappingStrategy)
  private
    FMerger           : TMappingTableMerger;
    FMappingStrategies: TList<IMappingStrategy>;
    FMappings         : TDictionary<TRttiType, TMappingTable>;
    function GetMapping(const AType: TRttiType): TMappingTable;
    procedure Add(const AMappingStrategy: IMappingStrategy);

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TFileMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    FMapping: ISuperObject;
    procedure GetMapping(const AType: TRttiType; ATable: TMappingTable);
    procedure ParseTable(ATable: TMappingTable; const AJsonTable: ISuperObject);
    procedure ParseIdField(ATable: TMappingTable; const AJsonTable: ISuperObject;
      const AType: TRttiType);
    procedure ParseField(const AField: TMappingField; const AJsonField: ISuperObject;
      const AType: TRttiType);
    procedure ParseHasMany(const PackageName: string; const AHasMany: TMappingRelation;
      const AJsonRelation: ISuperObject; const AType: TRttiType);
    procedure ParseHasOne(const PackageName: string; const AHasOne: TMappingRelation;
      const AJsonRelation: ISuperObject; const AType: TRttiType);
    procedure ParseBelongsTo(const PackageName: string; const ABelongsTo: TMappingBelongsTo;
      const AJsonRelation: ISuperObject; const AType: TRttiType);

  public
    constructor Create(const AJsonMapping: ISuperObject);
  end;

  TAttributesMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    function GetOrCreateField(const ATable: TMappingTable;
      AProp: TRttiProperty): TMappingField;
    procedure ParseTable(const AType: TRttiType; const ATable: TMappingTable);
    procedure ParseField(const AType: TRttiType; const ATable: TMappingTable;
      AProp: TRttiProperty);
    procedure ParseHasOne(const AType: TRttiType; const ATable: TMappingTable;
      AProp: TRttiProperty);
    procedure ParseHasMany(const AType: TRttiType; const ATable: TMappingTable;
      AProp: TRttiProperty);
    procedure ParseBelongsTo(const AType: TRttiType; const ATable: TMappingTable;
      AProp: TRttiProperty);
    procedure GetMapping(const AType: TRttiType; ATable: TMappingTable);
  end;

  TCoCMappingStrategy = class(TInterfacedObject, IMappingStrategy)
  private
    { Checks whether a given type is a candidate for a duck list.
      These conditions must be met:
      Have an Add method with a single argument of any type
      Have a Clear method with no argument
      Have a GetItem method with one argument of type Integer and that
      returns the same type used on Add method
      Have a Count property of type integer. }
    class function IsACollectionClass(const AType: TRttiType;
      out ElementType: TRttiType): Boolean;
    function SkipClassPrefix(const ClassName: string): string;
    class function GetMethod(const AType: TRttiType; const Name: string;
      const ArgCount: Integer; out AMethod: TRttiMethod): Boolean;
    procedure ParseTable(const AType: TRttiType; const ATable: TMappingTable);
    procedure ParseField(ATable: TMappingTable; AProp: TRttiProperty);
    procedure ParseHasOne(AType: TRttiType; ATable: TMappingTable;
      AProp: TRttiProperty);
    function ParseBelongsTo(AType: TRttiType; ATable: TMappingTable;
      AProp: TRttiProperty): Boolean;
    procedure ParseHasMany(AType, ACollectionItemType: TRttiType;
      ATable: TMappingTable; AProp: TRttiProperty);
    function ClassHasProperty(AType: TRttiType; const APropertyName: string): Boolean;
    procedure GetMapping(const AType: TRttiType; ATable: TMappingTable);
    function IsCoCEnabledForType(const AType: TRttiType): Boolean;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  dorm.Utils,
  Typinfo,
  dorm;

{ TCacheMappingStrategy }

procedure TCacheMappingStrategy.Add(const AMappingStrategy: IMappingStrategy);
begin
  FMappingStrategies.Add(AMappingStrategy);
end;

constructor TCacheMappingStrategy.Create;
begin
  inherited Create;
  FMerger := TMappingTableMerger.Create;
  FMappings := TDictionary<TRttiType, TMappingTable>.Create(128);
  FMappingStrategies := TList<IMappingStrategy>.Create;
end;

destructor TCacheMappingStrategy.Destroy;
var
  table: TMappingTable;
begin
  FMerger.Free;
  FMappingStrategies.Clear;
  FMappingStrategies.Free;
  for table in FMappings.Values do
    table.Free;
  FMappings.Free;
  inherited;
end;

function TCacheMappingStrategy.GetMapping(const AType: TRttiType): TMappingTable;
var
  tables: array of TMappingTable;
  i     : Integer;
begin
  if not FMappings.TryGetValue(AType, Result) then
  begin
    Result := TMappingTable.Create;
    SetLength(tables, FMappingStrategies.Count);
    for i := 0 to high(tables) do
      tables[i] := TMappingTable.Create;
    try
      for i := 0 to high(tables) do
        FMappingStrategies[i].GetMapping(AType, tables[i]);
      FMerger.Merge(Result, tables);
      if Result.TableName = EmptyStr then
        raise Exception.Create(Format('Cound not find mapping to Class %s', [AType.QualifiedName]));
      FMappings.Add(AType, Result);
    finally
      for i := 0 to high(tables) do
        tables[i].Free;
    end;
  end;
end;

{ TFileMappingStrategy }

constructor TFileMappingStrategy.Create(const AJsonMapping: ISuperObject);
begin
  inherited Create;
  FMapping := AJsonMapping;
end;

procedure TFileMappingStrategy.GetMapping(const AType: TRttiType; ATable: TMappingTable);
var
  jsonArray: TSuperArray;
  jsonTable: ISuperObject;
  i        : Integer;
  sl       : TStringList;
begin
  jsonTable := FMapping.O[AType.Name];
  if not Assigned(jsonTable) then
    Exit;

  ParseTable(ATable, jsonTable);

  ParseIdField(ATable, jsonTable, AType);

  jsonArray := jsonTable.O['fields'].AsArray;
  if Assigned(jsonArray) then
  begin
    for i := 0 to jsonArray.Length - 1 do
      ParseField(ATable.AddField, jsonArray[i], AType);
  end;

  jsonArray := jsonTable.A['has_one'];
  if Assigned(jsonArray) then
    for i := 0 to jsonArray.Length - 1 do
      ParseHasOne(jsonTable.S['package'], ATable.AddHasOne, jsonArray[i], AType);

  jsonArray := jsonTable.A['has_many'];
  if Assigned(jsonArray) then
    for i := 0 to jsonArray.Length - 1 do
      ParseHasMany(jsonTable.S['package'], ATable.AddHasMany, jsonArray[i], AType);

  jsonArray := jsonTable.A['belongs_to'];
  if Assigned(jsonArray) then
    for i := 0 to jsonArray.Length - 1 do
      ParseBelongsTo(jsonTable.S['package'], ATable.AddBelongsTo, jsonArray[i], AType);
end;

procedure TFileMappingStrategy.ParseTable(ATable: TMappingTable;
  const AJsonTable: ISuperObject);
begin
  ATable.TableName := AJsonTable.S['table'];
  ATable.Package := AJsonTable.S['package'];
end;

procedure TFileMappingStrategy.ParseIdField(ATable: TMappingTable;
  const AJsonTable: ISuperObject; const AType: TRttiType);
var
  idField    : TMappingField;
  jsonIDField: ISuperObject;
begin
  jsonIDField := AJsonTable.O['id'];
  if Assigned(jsonIDField) then
  begin
    idField := ATable.AddField;
    ParseField(idField, jsonIDField, AType);
    idField.IsPK := True;
  end;
end;

procedure TFileMappingStrategy.ParseField(const AField: TMappingField;
  const AJsonField: ISuperObject; const AType: TRttiType);
var
  indexType: string;
  RTTICache: TMappingCache;
begin
  AField.Name := AJsonField.S['name'];
  AField.FieldName := AJsonField.S['field'];
  AField.Size := AJsonField.i['size'];
  AField.FieldType := AJsonField.S['field_type'];
  AField.DefaultValue := AJsonField.S['default_value'];
  AField.Precision := AJsonField.i['precision'];
  AField.indexType := itNone;
  indexType := AJsonField.S['index_type'];
  if SameText(indexType, 'index') then
    AField.indexType := itIndex
  else if SameText(indexType, 'unique') then
    AField.indexType := itUnique;
  AField.IsPK := false;

  if not Assigned(AType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]',
      [AType.ToString]);

  RTTICache.RTTIField := AType.GetField(FieldFor(AField.Name));
  RTTICache.RTTIProp := AType.GetProperty(AField.Name);
  AField.RTTICache := RTTICache;

  { if not Assigned(AField.RTTIField) then
    begin
    AField.RTTIProp := AType.GetProperty(AField.Name);
    if not Assigned(AField.RTTIProp) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]',
    [AType.ToString, AField.Name]);
    end; }
end;

procedure TFileMappingStrategy.ParseHasOne(const PackageName: string;
  const AHasOne: TMappingRelation;
  const AJsonRelation: ISuperObject; const AType: TRttiType);
var
  RTTICache: TMappingCache;
begin
  AHasOne.Name := AJsonRelation.S['name'];
  AHasOne.ChildClassName := PackageName + '.' + AJsonRelation.S['class_name'];
  AHasOne.ChildFieldName := AJsonRelation.S['child_field_name'];
  AHasOne.LazyLoad := AJsonRelation.B['lazy_load'];

  RTTICache.RTTIField := AType.GetField(FieldFor(AHasOne.Name));
  RTTICache.RTTIProp := AType.GetProperty(AHasOne.Name);
  AHasOne.RTTICache := RTTICache;
end;

procedure TFileMappingStrategy.ParseHasMany(const PackageName: string;
  const AHasMany     : TMappingRelation;
  const AJsonRelation: ISuperObject; const AType: TRttiType);
var
  RTTICache: TMappingCache;
begin
  AHasMany.Name := AJsonRelation.S['name'];
  AHasMany.ChildClassName := PackageName + '.' + AJsonRelation.S['class_name'];
  AHasMany.ChildFieldName := AJsonRelation.S['child_field_name'];
  AHasMany.LazyLoad := AJsonRelation.B['lazy_load'];

  RTTICache.RTTIField := AType.GetField(FieldFor(AHasMany.Name));
  RTTICache.RTTIProp := AType.GetProperty(AHasMany.Name);
  AHasMany.RTTICache := RTTICache;
end;

procedure TFileMappingStrategy.ParseBelongsTo(const PackageName: string;
  const ABelongsTo: TMappingBelongsTo;
  const AJsonRelation: ISuperObject; const AType: TRttiType);
var
  RTTICache: TMappingCache;
begin
  ABelongsTo.Name := AJsonRelation.S['name'];
  ABelongsTo.OwnerClassName := PackageName + '.' + AJsonRelation.S['class_name'];
  ABelongsTo.RefFieldName := AJsonRelation.S['ref_field_name'];
  ABelongsTo.LazyLoad := AJsonRelation.B['lazy_load'];

  RTTICache.RTTIField := AType.GetField(FieldFor(ABelongsTo.Name));
  RTTICache.RTTIProp := AType.GetProperty(ABelongsTo.Name);
  ABelongsTo.RTTICache := RTTICache;
end;

{ TAttributesMappingStrategy }

procedure TAttributesMappingStrategy.GetMapping(const AType: TRttiType;
  ATable: TMappingTable);
var
  prop: TRttiProperty;
begin
  ParseTable(AType, ATable);
  for prop in AType.GetProperties do
  begin
    ParseField(AType, ATable, prop);
    ParseHasOne(AType, ATable, prop);
    ParseHasMany(AType, ATable, prop);
    ParseBelongsTo(AType, ATable, prop);
  end;
end;

procedure TAttributesMappingStrategy.ParseTable(const AType: TRttiType;
  const ATable: TMappingTable);
var
  attribute: Entity;
begin
  attribute := TdormUtils.GetAttribute<Entity>(AType);
  if Assigned(attribute) then
  begin
    ATable.Package := attribute.Package;
    ATable.TableName := attribute.TableName;
  end;
end;

procedure TAttributesMappingStrategy.ParseField(const AType: TRttiType;
  const ATable: TMappingTable; AProp: TRttiProperty);
var
  field    : TMappingField;
  attribute: TCustomAttribute;
  C        : Column;
  t        : TRttiType;
begin
  if TdormUtils.HasAttribute<Transient>(AProp) then
    Exit;

  attribute := TdormUtils.GetAttribute<Id>(AProp);
  if Assigned(attribute) then
  begin
    field := GetOrCreateField(ATable, AProp);
    field.IsPK := True;
  end;

  attribute := TdormUtils.GetAttribute<Column>(AProp);
  if Assigned(attribute) then
  begin
    field := GetOrCreateField(ATable, AProp);
    C := Column(attribute);
    field.FieldName := C.FieldName;
    field.Size := C.Size;
    field.Precision := C.Precision;
    field.DefaultValue := C.DefaultValue;
    if C.FieldType <> '' then // if attribute averride the inferred type...
      field.FieldType := C.FieldType; // user the type defined in the attribute
  end;

  attribute := TdormUtils.GetAttribute<Size>(AProp);
  if Assigned(attribute) then
  begin
    field := GetOrCreateField(ATable, AProp);
    with Size(attribute) do
    begin
      field.Size := ColumnSize;
      if ColumnPrecision <> 0 then
        field.Precision := ColumnPrecision
    end;
  end;

  attribute := TdormUtils.GetAttribute<DefaultValue>(AProp);
  if Assigned(attribute) then
  begin
    field := GetOrCreateField(ATable, AProp);
    field.DefaultValue := DefaultValue(attribute).Value;
  end;

end;

procedure TAttributesMappingStrategy.ParseHasOne(const AType: TRttiType;
  const ATable: TMappingTable; AProp: TRttiProperty);
var
  hasOneAttribute: TCustomAttribute;
  isLazy         : Boolean;
  relation       : TMappingRelation;
  RTTICache      : TMappingCache;
begin
  isLazy := TdormUtils.HasAttribute<Lazy>(AProp);
  hasOneAttribute := TdormUtils.GetAttribute<HasOne>(AProp);
  if Assigned(hasOneAttribute) then
  begin
    relation := ATable.AddHasOne;
    relation.Name := AProp.Name;
    // relation.ChildClassName := AProp.PropertyType.AsInstance.MetaclassType.ClassName;
    relation.ChildClassName := AProp.PropertyType.AsInstance.MetaclassType.QualifiedClassName;
    relation.ChildFieldName := HasOne(hasOneAttribute).ChildPropertyName;
    relation.LazyLoad := isLazy or HasMany(hasOneAttribute).LazyLoad;
    RTTICache.RTTIProp := AProp;
    relation.RTTICache := RTTICache;
  end;
end;

procedure TAttributesMappingStrategy.ParseHasMany(const AType: TRttiType;
  const ATable: TMappingTable; AProp: TRttiProperty);
var
  hasManyAttribute: TCustomAttribute;
  relation        : TMappingRelation;
  isLazy          : Boolean;
  RTTICache       : TMappingCache;
  attr            : ListOf;
  _type_name      : string;
  AChildType      : TRttiType;
begin
  isLazy := TdormUtils.HasAttribute<Lazy>(AProp);
  hasManyAttribute := TdormUtils.GetAttribute<HasMany>(AProp);
  if Assigned(hasManyAttribute) then
  begin
    relation := ATable.AddHasMany;
    relation.Name := AProp.Name;

    /// ////////////////////////////////////////////////////////////
    // When you have an HasMany relations declared as "TPhones = class(TObjectLlist<TPhone>)" dorm
    // needs to know the mapping of the internal object and not of the list object itself. So now we're
    // searching for an attribute named "ListOf" that explain to the RTTI engine what is internal object type
    attr := TdormUtils.GetAttribute<ListOf>(AProp);
    if Assigned(attr) then
    begin
      _type_name := attr.Value;
      AChildType := TdormUtils.ctx.FindType(_type_name);
      if not Assigned(AChildType) then
        raise Exception.Create('Unknown type ' + _type_name + ' (ListOf ' +
          attr.Value + ')');
      relation.ChildClassName := _type_name;
      // AChildType.AsInstance.MetaclassType.QualifiedClassName;
    end
    else
      /// ////////////////////////////////////////////////////////////
      relation.ChildClassName := AProp.PropertyType.AsInstance.MetaclassType.QualifiedClassName;
    relation.ChildFieldName := HasMany(hasManyAttribute).ChildPropertyName;
    relation.LazyLoad := isLazy or HasMany(hasManyAttribute).LazyLoad;
    RTTICache.RTTIProp := AProp;
    relation.RTTICache := RTTICache;
  end;
end;

procedure TAttributesMappingStrategy.ParseBelongsTo(const AType: TRttiType;
  const ATable: TMappingTable; AProp: TRttiProperty);
var
  belongsToAttribute: TCustomAttribute;
  relation          : TMappingBelongsTo;
  isLazy            : Boolean;
  RTTICache         : TMappingCache;
begin
  isLazy := TdormUtils.HasAttribute<Lazy>(AProp);
  belongsToAttribute := TdormUtils.GetAttribute<BelongsTo>(AProp);
  if Assigned(belongsToAttribute) then
  begin
    relation := ATable.AddBelongsTo;
    relation.Name := AProp.Name;
    // relation.OwnerClassName := AProp.PropertyType.AsInstance.MetaclassType.ClassName;
    relation.OwnerClassName := AProp.PropertyType.AsInstance.MetaclassType.QualifiedClassName;
    relation.RefFieldName := HasOne(belongsToAttribute).ChildPropertyName;
    relation.LazyLoad := isLazy or HasMany(belongsToAttribute).LazyLoad;
    RTTICache.RTTIProp := AProp;
    relation.RTTICache := RTTICache;
  end;
end;

function TAttributesMappingStrategy.GetOrCreateField(
  const ATable: TMappingTable; AProp: TRttiProperty): TMappingField;
begin
  Result := ATable.FindByName(AProp.Name);
  if not Assigned(Result) then
  begin
    Result := ATable.AddField;
    Result.Name := AProp.Name;
    Result.FieldType := TdormUtils.GetFieldType(AProp);
  end;
end;

{ TCoCMappingStrategy }

function TCoCMappingStrategy.IsCoCEnabledForType(const AType: TRttiType): Boolean;
var
  Attrs: TArray<TCustomAttribute>;
  attr : TObject;
begin
  Result := True;
  Attrs := AType.GetAttributes;
  for attr in Attrs do
    if attr is NoAutomapping then
      Exit(false);
end;

procedure TCoCMappingStrategy.GetMapping(const AType: TRttiType;
  ATable: TMappingTable);
var
  prop              : TRttiProperty;
  collectionItemType: TRttiType;
begin
  if not IsCoCEnabledForType(AType) then
    Exit;
  ParseTable(AType, ATable);
  for prop in AType.GetProperties do
    if (TdormUtils.HasAttribute<Transient>(prop) or TdormUtils.HasAttribute<NoAutomapping>(prop))
    then
      Continue
    else if prop.PropertyType.IsInstance then
    begin
      if IsACollectionClass(prop.PropertyType, collectionItemType) then
        ParseHasMany(AType, collectionItemType, ATable, prop)
      else if not ParseBelongsTo(AType, ATable, prop) then
        if prop.PropertyType.AsInstance.MetaclassType.InheritsFrom(TStream) then
          ParseField(ATable, prop)
        else
          ParseHasOne(AType, ATable, prop);
    end
    else if not prop.PropertyType.IsRecord and not prop.PropertyType.IsSet then
      ParseField(ATable, prop);
end;

procedure TCoCMappingStrategy.ParseTable(const AType: TRttiType;
  const ATable: TMappingTable);
begin
  ATable.TableName := AnsiUpperCase(SkipClassPrefix(AType.Name));
  ATable.Package := AType.AsInstance.MetaclassType.UnitName;
end;

procedure TCoCMappingStrategy.ParseField(ATable: TMappingTable; AProp: TRttiProperty);
var
  field    : TMappingField;
  FieldType: string;
  RTTICache: TMappingCache;
begin
  FieldType := TdormUtils.GetFieldType(AProp);
  field := ATable.AddField;
  if AnsiUpperCase(AProp.Name) = 'ID' then
    field.IsPK := True;
  field.Name := AProp.Name;
  field.FieldName := AnsiUpperCase(AProp.Name);
  field.FieldType := FieldType;

  RTTICache.RTTIField := nil;
  RTTICache.RTTIProp := AProp;
  field.RTTICache := RTTICache;
end;

procedure TCoCMappingStrategy.ParseHasOne(AType: TRttiType;
  ATable: TMappingTable; AProp: TRttiProperty);
var
  relation      : TMappingRelation;
  ChildFieldName: string;
begin
  ChildFieldName := SkipClassPrefix(AType.Name) + 'ID';
  if ClassHasProperty(AProp.PropertyType, ChildFieldName) then
  begin
    relation := ATable.AddHasOne;
    relation.Name := AProp.Name;
    relation.ChildClassName := AProp.PropertyType.QualifiedName;
    relation.ChildFieldName := ChildFieldName;
    relation.LazyLoad := false;
  end;
end;

function TCoCMappingStrategy.ParseBelongsTo(AType: TRttiType; ATable: TMappingTable;
  AProp: TRttiProperty): Boolean;
var
  relation    : TMappingBelongsTo;
  RefFieldName: string;
begin
  Result := false;
  if SameText(LeftStr(AProp.Name, 5), 'Owner') then
  begin
    RefFieldName := AProp.Name;
    Delete(RefFieldName, 1, 5);
    if RefFieldName = EmptyStr then
      RefFieldName := 'ID';
    RefFieldName := SkipClassPrefix(AProp.PropertyType.Name) + RefFieldName;
    if ClassHasProperty(AType, RefFieldName) then
    begin
      relation := ATable.AddBelongsTo;
      relation.Name := AProp.Name;
      relation.OwnerClassName := AProp.PropertyType.QualifiedName;
      relation.RefFieldName := RefFieldName;
      relation.LazyLoad := True;
      Result := True;
    end;
  end;
end;

procedure TCoCMappingStrategy.ParseHasMany(AType, ACollectionItemType: TRttiType;
  ATable: TMappingTable; AProp: TRttiProperty);
var
  relation      : TMappingRelation;
  ChildFieldName: string;
begin
  ChildFieldName := SkipClassPrefix(AType.Name) + 'ID';
  if ClassHasProperty(ACollectionItemType, ChildFieldName) then
  begin
    relation := ATable.AddHasMany;
    relation.Name := AProp.Name;
    relation.ChildClassName := ACollectionItemType.QualifiedName;
    relation.ChildFieldName := ChildFieldName;
    relation.LazyLoad := false;
  end;
end;

class
  function TCoCMappingStrategy.GetMethod(const AType: TRttiType; const Name: string;
  const ArgCount: Integer; out AMethod: TRttiMethod): Boolean;
begin
  AMethod := AType.GetMethod(name);
  Result := Assigned(AMethod) and (Length(AMethod.GetParameters) = ArgCount);
end;

function TCoCMappingStrategy.ClassHasProperty(AType: TRttiType;
  const APropertyName: string): Boolean;
var
  prop: TRttiProperty;
begin
  prop := AType.GetProperty(APropertyName);
  Result := Assigned(prop);
end;

class
  function TCoCMappingStrategy.IsACollectionClass(
  const AType: TRttiType; out ElementType: TRttiType): Boolean;
var
  method: TRttiMethod;
  prop  : TRttiProperty;
begin
  // Check Add
  if not GetMethod(AType, 'Add', 1, method) then
    Exit(false);
  ElementType := method.GetParameters[0].ParamType;

  // Check Clear
  if not GetMethod(AType, 'Clear', 0, method) then
    Exit(false);

  // Check GetItem
  if not GetMethod(AType, 'GetItem', 1, method) then
    Exit(false);
  if method.GetParameters[0].ParamType.Handle <> TypeInfo(Integer) then
    Exit(false);
  if method.ReturnType <> ElementType then
    Exit(false);

  // Check Count
  prop := AType.GetProperty('Count');
  if not Assigned(prop) then
    Exit(false);
  if prop.PropertyType.Handle <> TypeInfo(Integer) then
    Exit(false);

  Result := True;
end;

function TCoCMappingStrategy.SkipClassPrefix(const ClassName: string): string;
begin
  Result := Copy(ClassName, 2);
end;

{ TMappingTableMerger }

procedure TMappingTableMerger.Merge(AOutput: TMappingTable;
  AInput: array of TMappingTable);
var
  tableToMerge    : TMappingTable;
  fieldToMerge    : TMappingField;
  hasOneToMerge   : TMappingRelation;
  hasManyToMerge  : TMappingRelation;
  belongsToToMerge: TMappingBelongsTo;
begin
  for tableToMerge in AInput do
  begin
    if AOutput.TableName = EmptyStr then
      AOutput.TableName := tableToMerge.TableName;

    if AOutput.Package = EmptyStr then
      AOutput.Package := tableToMerge.Package;

    for fieldToMerge in tableToMerge.Fields do
    begin
      if MergeFieldTo(fieldToMerge, AOutput, tableToMerge) then
        Continue;
      AOutput.AddField.Assign(fieldToMerge);
    end;

    for hasOneToMerge in tableToMerge.HasOneList do
    begin
      if MergeHasOneTo(hasOneToMerge, AOutput) then
        Continue;
      AOutput.AddHasOne.Assign(hasOneToMerge);
    end;

    for hasManyToMerge in tableToMerge.HasManyList do
    begin
      if MergeHasManyTo(hasManyToMerge, AOutput) then
        Continue;
      AOutput.AddHasMany.Assign(hasManyToMerge);
    end;

    for belongsToToMerge in tableToMerge.BelongsToList do
    begin
      if MergeBelongsTo(belongsToToMerge, AOutput) then
        Continue;
      AOutput.AddBelongsTo.Assign(belongsToToMerge);
    end;

  end;
end;

function TMappingTableMerger.MergeFieldTo(AField: TMappingField;
  AOutput, AInput: TMappingTable): Boolean;
var
  outPutField: TMappingField;
begin
  outPutField := AOutput.FindByName(AField.Name);
  Result := Assigned(outPutField);
  if Result then
  begin
    if (outPutField.FieldName = EmptyStr)
      and not SameText(outPutField.FieldName, AField.FieldName) then
      outPutField.FieldName := AField.FieldName;

    if (outPutField.FieldType = EmptyStr)
      and not SameText(outPutField.FieldType, AField.FieldType) then
      outPutField.FieldType := AField.FieldType;

    if (outPutField.DefaultValue = EmptyStr)
      and not SameText(outPutField.DefaultValue, AField.DefaultValue) then
      outPutField.DefaultValue := AField.DefaultValue;

    if (outPutField.Size = 0)
      and (outPutField.Size <> AField.Size) then
      outPutField.Size := AField.Size;

    if (outPutField.Precision = 0)
      and (outPutField.Precision <> AField.Precision) then
      outPutField.Precision := AField.Precision;

    if (outPutField.indexType = itNone)
      and (outPutField.indexType <> AField.indexType) then
      outPutField.indexType := AField.indexType;

    if not outPutField.IsPK and AField.IsPK then
      outPutField.IsPK := AField.IsPK;

    if ((not Assigned(outPutField.RTTICache.RTTIField)) and (Assigned((AField.RTTICache.RTTIField)))
      ) or
      ((not Assigned(outPutField.RTTICache.RTTIProp)) and (Assigned((AField.RTTICache.RTTIProp))))
    then
      outPutField.RTTICache := AField.RTTICache;
  end;
end;

function TMappingTableMerger.MergeRelation(AInRelation, AOutRelation: TMappingRelation): Boolean;
begin
  Result := Assigned(AOutRelation);
  if Result then
  begin
    if (Trim(AOutRelation.ChildClassName) = EmptyStr)
      and not SameText(AOutRelation.ChildClassName, AInRelation.ChildClassName) then
      AOutRelation.ChildClassName := AInRelation.ChildClassName;

    if (Trim(AOutRelation.ChildFieldName) = EmptyStr)
      and not SameText(AOutRelation.ChildFieldName, AInRelation.ChildFieldName) then
      AOutRelation.ChildFieldName := AInRelation.ChildFieldName;

    // for while we will treat the lazy but can be necessary treat the joincolumn field too
    if not AOutRelation.LazyLoad and AInRelation.LazyLoad then
      AOutRelation.LazyLoad := AInRelation.LazyLoad;
  end;
end;

function TMappingTableMerger.MergeHasOneTo(ARelation: TMappingRelation;
  AOutput: TMappingTable): Boolean;
var
  outRelation: TMappingRelation;
begin
  outRelation := AOutput.FindHasOneByName(ARelation.Name);
  Result := MergeRelation(ARelation, outRelation)
end;

function TMappingTableMerger.MergeHasManyTo(ARelation: TMappingRelation;
  AOutput: TMappingTable): Boolean;
var
  outRelation: TMappingRelation;
begin
  outRelation := AOutput.FindHasManyByName(ARelation.Name);
  Result := MergeRelation(ARelation, outRelation)
end;

function TMappingTableMerger.MergeBelongsTo(ARelation: TMappingBelongsTo;
  AOutput: TMappingTable): Boolean;
var
  outRelation: TMappingBelongsTo;
begin
  outRelation := AOutput.FindBelongsToByName(ARelation.Name);
  Result := Assigned(outRelation);
  if Result then
  begin
    if (Trim(outRelation.OwnerClassName) = EmptyStr)
      and not SameText(outRelation.OwnerClassName, ARelation.OwnerClassName) then
      outRelation.OwnerClassName := ARelation.OwnerClassName;

    if (Trim(outRelation.RefFieldName) = EmptyStr)
      and not SameText(outRelation.RefFieldName, ARelation.RefFieldName) then
      outRelation.RefFieldName := ARelation.RefFieldName;

    // for while we will treat the lazy but can be necessary treat the joincolumn field too
    if not outRelation.LazyLoad and ARelation.LazyLoad then
      outRelation.LazyLoad := ARelation.LazyLoad;
  end;
end;

end.
