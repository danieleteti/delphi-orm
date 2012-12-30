unit dorm.Mappings;

interface

uses
  Rtti,
  Generics.Collections,
  superobject;

type
{$RTTI EXPLICIT
    FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])
    METHODS([vcPrivate, vcProtected, vcPublic, vcPublished])
    PROPERTIES([vcPrivate, vcProtected, vcPublic, vcPublished])}
  // Mapping Attibutes
  Entity = class(TCustomAttribute)
  private
    FTableName: String;
    FPackage: String;
  public
    constructor Create(const ATableName: String = '';
      const APackageName: String = '');
    property TableName: String read FTableName;
    property Package: String read FPackage;
  end;

  Column = class(TCustomAttribute)
  private
    FFieldName: string;
    FFieldType: string;
    FSize: Cardinal;
    FPrecision: Cardinal;
    FDefaultValue: string;
  public
    constructor Create; overload;
    constructor Create(const AColumnName: String; ASize: Cardinal = 0;
      APrecision: Cardinal = 0; ADefaultValue: String = ''); overload;
    constructor Create(const AColumnName: string; AFieldType: String;
      ASize: Cardinal = 0; APrecision: Cardinal = 0;
      ADefaultValue: String = ''); overload;
    property FieldName: string read FFieldName write FFieldName;
    property FieldType: string read FFieldType write FFieldType;
    property Size: Cardinal read FSize write FSize;
    property Precision: Cardinal read FPrecision write FPrecision;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
  end;

  NoAutomapping = class(TCustomAttribute)
  end;

  Size = class(TCustomAttribute)
  private
    FColumnSize: Cardinal;
    FColumnPrecision: Cardinal;
  public
    constructor Create(const ASize: Cardinal; const APrecision: Cardinal = 0);
    property ColumnSize: Cardinal read FColumnSize;
    property ColumnPrecision: Cardinal read FColumnPrecision;
  end;

  DefaultValue = class(TCustomAttribute)
  private
    FValue: String;
  public
    constructor Create(const ADefaultValue: String);
    property Value: String read FValue;
  end;

  ListOf = class(DefaultValue)
  end;

  TCustomRelationAttribute = class(TCustomAttribute)
  private
    FChildPropertyName: String;
    FLazyLoad: boolean;
  public
    constructor Create(const AChildPropertyName: String;
      ALazyLoad: boolean = False);
    property ChildPropertyName: String read FChildPropertyName;
    property LazyLoad: boolean read FLazyLoad;
  end;

  Lazy = class(TCustomAttribute);
  HasOne = class(TCustomRelationAttribute);
  HasMany = class(TCustomRelationAttribute);

  BelongsTo = class(TCustomAttribute)
  private
    FRefPropertyName: String;
    FLazyLoad: boolean;
  public
    constructor Create(const ARefPropertyName: String;
      ALazyLoad: boolean = False);
    property RefPropertyName: String read FRefPropertyName;
    property LazyLoad: boolean read FLazyLoad;
  end;

  Id = class(TCustomAttribute);
  Transient = class(TCustomAttribute);

  // Mapping classes
  TdormIndexType = (itNone, itIndex, itUnique);
  TdormKeyType = (ktInteger, ktString);
  TMappingTable = class;

  TMappingField = class
  private
    FPK: boolean;
    FName: string;
    FFieldType: string;
    FDefaultValue: string;
    FIndexType: TdormIndexType;
    FPrecision: Cardinal;
    FFieldName: string;
    FSize: Cardinal;
  public
    constructor Create;
    procedure Assign(Source: TMappingField);
    function ToString: string; override;
    property Name: string read FName write FName;
    property FieldName: string read FFieldName write FFieldName;
    property FieldType: string read FFieldType write FFieldType;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property Size: Cardinal read FSize write FSize;
    property Precision: Cardinal read FPrecision write FPrecision;
    property IndexType: TdormIndexType read FIndexType write FIndexType;
    property IsPK: boolean read FPK write FPK;
  end;

  TMappingRelation = class
  private
    FName: string;
    FChildClassName: string;
    FChildFieldName: string;
    FLazyLoad: boolean;
  public
    property Name: string read FName write FName;
    property ChildClassName: string read FChildClassName write FChildClassName;
    property ChildFieldName: string read FChildFieldName write FChildFieldName;
    property LazyLoad: boolean read FLazyLoad write FLazyLoad;
    procedure Assign(Source: TMappingRelation);
  end;

  TMappingBelongsTo = class
  private
    FName: string;
    FOwnerClassName: string;
    FRefFieldName: string;
    FLazyLoad: boolean;
  public
    property Name: string read FName write FName;
    property OwnerClassName: string read FOwnerClassName write FOwnerClassName;
    property RefFieldName: string read FRefFieldName write FRefFieldName;
    property LazyLoad: boolean read FLazyLoad write FLazyLoad;
    procedure Assign(Source: TMappingBelongsTo);
  end;

  TMappingFieldList = class(TObjectList<TMappingField>);
  TMappingRelationList = class(TObjectList<TMappingRelation>);
  TMappingBelongsToList = class(TObjectList<TMappingBelongsTo>);

  TMappingTable = class
  private
    FPackage: string;
    FTableName: string;
    FFields: TMappingFieldList;
    FBelongsToList: TMappingBelongsToList;
    FHasManyList: TMappingRelationList;
    FHasOneList: TMappingRelationList;
    function GetId: TMappingField;
  public
    constructor Create;
    destructor Destroy; override;
    function FindByName(const AName: string): TMappingField;
    function FindHasOneByName(const AName: string): TMappingRelation;
    function FindHasManyByName(const AName: string): TMappingRelation;
    function FindBelongsToByName(const AName: string): TMappingBelongsTo;
    function AddField: TMappingField;
    function AddBelongsTo: TMappingBelongsTo;
    function AddHasMany: TMappingRelation;
    function AddHasOne: TMappingRelation;
    function ToString: string; override;
    property Package: string read FPackage write FPackage;
    property TableName: string read FTableName write FTableName;
    property Id: TMappingField read GetId;
    property Fields: TMappingFieldList read FFields;
    property HasManyList: TMappingRelationList read FHasManyList;
    property HasOneList: TMappingRelationList read FHasOneList;
    property BelongsToList: TMappingBelongsToList read FBelongsToList;
  end;

implementation

uses
  SysUtils,
  Variants,
  StrUtils,
  dorm.Utils,
  dorm.Commons;
{ Entity }

constructor Entity.Create(const ATableName: String = '';
  const APackageName: String = '');
begin
  FTableName := ATableName;
  FPackage := APackageName;
end;
{ Column }

constructor Column.Create;
begin
  inherited Create;
  FFieldName := EmptyStr; // must be EmptyStr
  FFieldType := EmptyStr; // must be EmptyStr
  FDefaultValue := EmptyStr; // must be EmptyStr
  FSize := 0;
  FPrecision := 0;
end;

constructor Column.Create(const AColumnName: String; ASize: Cardinal = 0;
  APrecision: Cardinal = 0; ADefaultValue: String = '');
begin
  Create;
  FSize := ASize;
  FFieldName := AColumnName;
  FPrecision := APrecision;
  FDefaultValue := ADefaultValue;
end;

constructor Column.Create(const AColumnName: string; AFieldType: String;
  ASize: Cardinal = 0; APrecision: Cardinal = 0; ADefaultValue: String = '');
begin
  Create(AColumnName, ASize, APrecision, ADefaultValue);
  FFieldType := AFieldType;
end;
{ Size }

constructor Size.Create(const ASize: Cardinal; const APrecision: Cardinal = 0);
begin
  inherited Create;
  FColumnSize := ASize;
  FColumnPrecision := APrecision;
end;
{ DefaultValue }

constructor DefaultValue.Create(const ADefaultValue: String);
begin
  inherited Create;
  FValue := ADefaultValue;
end;
{ TCustomRelationAttribute }

constructor TCustomRelationAttribute.Create(const AChildPropertyName: String;
  ALazyLoad: boolean = False);
begin
  inherited Create;
  FChildPropertyName := AChildPropertyName;
  FLazyLoad := ALazyLoad;
end;
{ BelongsTo }

constructor BelongsTo.Create(const ARefPropertyName: String;
  ALazyLoad: boolean = False);
begin
  inherited Create;
  FRefPropertyName := ARefPropertyName;
  FLazyLoad := ALazyLoad;
end;
{ TMappingTable }

constructor TMappingTable.Create;
begin
  FFields := TMappingFieldList.Create;
  FHasOneList := TMappingRelationList.Create;
  FHasManyList := TMappingRelationList.Create;
  FBelongsToList := TMappingBelongsToList.Create;
  FTableName := '';
  FPackage := '';
end;

destructor TMappingTable.Destroy;
begin
  FFields.Free;
  FHasManyList.Free;
  FHasOneList.Free;
  FBelongsToList.Free;
  inherited;
end;

function TMappingTable.FindBelongsToByName(const AName: string)
  : TMappingBelongsTo;
var
  _Rel: TMappingBelongsTo;
begin
  Result := nil;
  for _Rel in BelongsToList do
    if AnsiSameText(_Rel.Name, AName) then
      Exit(_Rel);
end;

function TMappingTable.FindByName(const AName: string): TMappingField;
var
  _Field: TMappingField;
begin
  Result := nil;
  for _Field in Fields do
    if AnsiSameText(_Field.Name, AName) then
      Exit(_Field);
end;

// function TMappingTable.GetFieldByAttribute(const AAttributeName: string): TMappingField;
// var
// fm: TMappingField;
// begin
// for fm in Fields do
// if fm.Name = AAttributeName then
// Exit(fm);
// raise EdormException.CreateFmt('Unknown field attribute %s', [AAttributeName]);
// end;

function TMappingTable.FindHasManyByName(const AName: string): TMappingRelation;
var
  _Rel: TMappingRelation;
begin
  Result := nil;
  for _Rel in HasManyList do
    if AnsiSameText(_Rel.Name, AName) then
      Exit(_Rel);
end;

function TMappingTable.FindHasOneByName(const AName: string): TMappingRelation;
var
  _Rel: TMappingRelation;
begin
  Result := nil;
  for _Rel in HasOneList do
    if AnsiSameText(_Rel.Name, AName) then
      Exit(_Rel);
end;

function TMappingTable.GetId: TMappingField;
var
  F: TMappingField;
begin
  Result := Nil;
  for F in Fields do
    if F.IsPK then
      Exit(F);
end;

function TMappingTable.AddBelongsTo: TMappingBelongsTo;
begin
  Result := TMappingBelongsTo.Create;
  FBelongsToList.Add(Result);
end;

function TMappingTable.AddField: TMappingField;
begin
  Result := TMappingField.Create;
  FFields.Add(Result);
end;

function TMappingTable.AddHasMany: TMappingRelation;
begin
  Result := TMappingRelation.Create;
  FHasManyList.Add(Result);
end;

function TMappingTable.AddHasOne: TMappingRelation;
begin
  Result := TMappingRelation.Create;
  FHasOneList.Add(Result);
end;

function TMappingTable.ToString: string;
begin
  Result := Format('Package: %s,  Table: %s', [Package, TableName]);
end;
{ TMappingField }

procedure TMappingField.Assign(Source: TMappingField);
begin
  FPK := Source.IsPK;
  FName := Source.Name;
  FFieldType := Source.FieldType;
  FDefaultValue := Source.DefaultValue;
  FIndexType := Source.IndexType;
  FPrecision := Source.Precision;
  FFieldName := Source.FieldName;
  FSize := Source.Size;
end;

constructor TMappingField.Create;
begin
  inherited;
  FName := '';
  FFieldName := '';
  FFieldType := '';
  FDefaultValue := '';
  FSize := 0;
  FPrecision := 0;
  FIndexType := itNone;
end;

function TMappingField.ToString: string;
begin
  Result := Format('PK: %s, name: %s, field: %s, field type: %s',
    [BoolToStr(IsPK, True), name, FieldName, FieldType]);
end;
{ TMappingRelation }

procedure TMappingRelation.Assign(Source: TMappingRelation);
begin
  FName := Source.Name;
  FChildClassName := Source.ChildClassName;
  FChildFieldName := Source.ChildFieldName;
  FLazyLoad := Source.LazyLoad;
end;
{ TMappingBelongsTo }

procedure TMappingBelongsTo.Assign(Source: TMappingBelongsTo);
begin
  FName := Source.Name;
  FOwnerClassName := Source.OwnerClassName;
  FRefFieldName := Source.RefFieldName;
  FLazyLoad := Source.LazyLoad;
end;

end.
