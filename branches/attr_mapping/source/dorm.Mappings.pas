unit dorm.Mappings;

interface

uses
  Rtti,
  Generics.Collections,
  dorm.Commons,
  superobject;

type
  // Mapping Attibutes
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

  // Mapping classes
  TMappingField = class
  private
    FName: string;
    FFieldType: string;
    FDefaultValue: string;
    FIndexType: TdormIndexType;
    FPrecision: Cardinal;
    FFieldName: string;
    FSize: Cardinal;
  public
    property Name: string read FName write FName;
    property FieldName: string read FFieldName write FFieldName;
    property FieldType: string read FFieldType write FFieldType;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property Size: Cardinal read FSize write FSize;
    property Precision: Cardinal read FPrecision write FPrecision;
    property IndexType: TdormIndexType read FIndexType write FIndexType;
  end;

  TMappingRelation = class
  private
    FName: string;
    FClassName: string;
    FChildFieldName: string;
    FLazyLoad: boolean;
  public
    property Name: string read FName write FName;
    property ClassName: string read FClassName write FClassName;
    property ChildFieldName: string read FChildFieldName write FChildFieldName;
    property LazyLoad: boolean read FLazyLoad write FLazyLoad;
  end;

  TMappingRelationBelongTo = class
  private
    FName: string;
    FClassName: string;
    FRefFieldName: string;
    FLazyLoad: boolean;
  public
    property Name: string read FName write FName;
    property ClassName: string read FClassName write FClassName;
    property RefFieldName: string read FRefFieldName write FRefFieldName;
    property LazyLoad: boolean read FLazyLoad write FLazyLoad;
  end;

  TMappingTable = class
  private
    FId: TMappingField;
    FPackage: string;
    FTableName: string;
    FFields: TObjectList<TMappingField>;
    FBelongTo: TObjectList<TMappingRelationBelongTo>;
    FHasMany: TObjectList<TMappingRelation>;
    FHasOne: TObjectList<TMappingRelation>;
    FAddFields: TMappingField;
    FAddHasMany: TMappingRelation;
    FAddBelongTo: TMappingRelationBelongTo;
    FAddHasOne: TMappingRelation;
    function GetBelongTo: TObjectList<TMappingRelationBelongTo>;
    function GetHasMany: TObjectList<TMappingRelation>;
    function GetHasOne: TObjectList<TMappingRelation>;
    function GetFields: TObjectList<TMappingField>;
  public
    constructor Create;
    destructor Destroy; override;
    function AddFields: TMappingField;
    function AddBelongTo: TMappingRelationBelongTo;
    function AddHasMany: TMappingRelation;
    function AddHasOne: TMappingRelation;
    property Package: string read FPackage write FPackage;
    property TableName: string read FTableName write FTableName;
    property Id: TMappingField read FId write FId;
    property Fields: TObjectList<TMappingField> read GetFields;
    property HasMany: TObjectList<TMappingRelation> read GetHasMany;
    property HasOne: TObjectList<TMappingRelation> read GetHasOne;
    property BelongTo: TObjectList<TMappingRelationBelongTo> read GetBelongTo;
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

{ TMappingTable }

constructor TMappingTable.Create;
begin
end;

destructor TMappingTable.Destroy;
begin
  FFields.Free;
  FHasMany.Free;
  FHasOne.Free;
  FBelongTo.Free;
  inherited;
end;

function TMappingTable.GetBelongTo: TObjectList<TMappingRelationBelongTo>;
begin
  if not Assigned(FBelongTo) then
    FBelongTo := TObjectList<TMappingRelationBelongTo>.Create;
  Result := FBelongTo;
end;

function TMappingTable.GetFields: TObjectList<TMappingField>;
begin
  if not Assigned(FFields) then
    FFields := TObjectList<TMappingField>.Create;
  Result := FFields;
end;

function TMappingTable.GetHasMany: TObjectList<TMappingRelation>;
begin
  if not Assigned(FHasMany) then
    FHasMany := TObjectList<TMappingRelation>.Create;
  Result := FHasMany;
end;

function TMappingTable.GetHasOne: TObjectList<TMappingRelation>;
begin
  if not Assigned(FHasOne) then
    FHasOne := TObjectList<TMappingRelation>.Create;
  Result := FHasOne;
end;

function TMappingTable.AddBelongTo: TMappingRelationBelongTo;
begin
  Result := TMappingRelationBelongTo.Create;
  FBelongTo.Add(Result);
end;

function TMappingTable.AddFields: TMappingField;
begin
  Result := TMappingField.Create;
  FFields.Add(Result);
end;

function TMappingTable.AddHasMany: TMappingRelation;
begin
  Result := TMappingRelation.Create;
  FHasMany.Add(Result);
end;

function TMappingTable.AddHasOne: TMappingRelation;
begin
  Result := TMappingRelation.Create;
  FHasOne.Add(Result);
end;

end.
