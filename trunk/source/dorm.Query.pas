unit dorm.Query;

interface

uses Generics.Collections,
  dorm.Mappings.Strategies,
  System.Rtti,
  dorm.Filters,
  dorm.Commons,
  System.TypInfo, dorm.ObjectStatus;

type
  TdormParameterType = (TypeString, TypeInteger, TypeFloat, TypeBoolean,
    TypeDate, TypeDataTime, TypeDecimal);

  TdormParameter = class
  private
    FAsString: string;
    FStrategy: IdormPersistStrategy;
    procedure SetStrategy(const Value: IdormPersistStrategy);
    procedure SetValue(const Value: TValue);

  strict protected
    FParameterType: TdormParameterType;
    FValue: TValue;

  public
    constructor Create(AParameterType: TdormParameterType; AValue: TValue);
    function AsString(AMapping: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): string;
    property Strategy: IdormPersistStrategy read FStrategy write SetStrategy;
  end;

  ISQLable = interface
    ['{63C9E9C1-B023-4550-B848-ACD63F2B8051}']
    function ToSQL(AMappingStrategy: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): ICustomCriteria;
  end;

  IdormSession = interface(IInvokable)
    ['{74BA59D1-0EF5-471C-9A29-BAD3E55B7627}']
    procedure CopyObject(SourceObject, TargetObject: TObject);
    function OIDIsSet(AObject: TObject): boolean;
    procedure ClearOID(AObject: TObject);
    function Persist(AObject: TObject): TObject;
    function PersistCollection(ACollection: TObject): TObject;
    procedure SetObjectStatus(AObject: TObject; AStatus: TdormObjectStatus;
      ARaiseExceptionIfNotExists: boolean = true);
    function IsDirty(AObject: TObject): boolean;
    function IsClean(AObject: TObject): boolean;
    function IsUnknown(AObject: TObject): boolean;
    procedure LoadRelations(AObject: TObject;
      ARelationsSet: TdormRelations = [drBelongsTo, drHasMany, drHasOne];
      AConsiderLazyLoading: boolean = true); overload;
    procedure LoadRelationsForEachElement(AList: TObject;
      ARelationsSet: TdormRelations = [drBelongsTo, drHasMany, drHasOne];
      AConsiderLazyLoading: boolean = true); overload;
    { Load methods }
    function Load(ATypeInfo: PTypeInfo; const Value: TValue; AObject: TObject)
      : boolean; overload;
    function Load(AClassType: TClass; Criteria: ICriteria; out AObject: TObject)
      : boolean; overload;
    function Load(ATypeInfo: PTypeInfo; const Value: TValue): TObject; overload;
    function Load(APTypeInfo: PTypeInfo; ASQLable: ISQLable): TObject; overload;
    procedure LoadList(AClassType: TClass; Criteria: ICriteria;
      ACollection: TObject); overload;
    function LoadList(AClassType: TClass; Criteria: ICriteria = nil):
{$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND};
      overload;
    procedure FillListSQL(APTypeInfo: PTypeInfo; ACollection: TObject;
      ASQLable: ISQLable); overload;
    procedure FillList(APTypeInfo: PTypeInfo; ACollection: TObject;
      ACriteria: ICriteria = nil); overload;
    // COMMANDERS
    function ExecuteCommand(ACommand: IdormCommand): Int64;
    // Other stuff
    procedure EnableLazyLoad(AClass: TClass; const APropertyName: string);
    procedure DisableLazyLoad(AClass: TClass; const APropertyName: string);
    function Count(AClassType: TClass; ACriteria: ICriteria = nil): Int64;
    procedure DeleteAll(AClassType: TClass);
    // transaction
    procedure StartTransaction;
    procedure Commit(RestartAfterCommit: boolean = false);
    procedure Rollback;
    function IsInTransaction: boolean;
  end;

  TFrom = class;

  TSelect = class(TInterfacedObject, ISQLable)
  private
    FDistinct: boolean;
    FAll: boolean;
    FScope: TDictionary<string, TRttiType>;

  protected
    constructor Create; virtual;

  public
    function ToSQL(AMappingStrategy: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): ICustomCriteria;
    function All: TSelect;
    function Distinct: TSelect;
    function From(AEntity: TClass): TFrom;
    destructor Destroy; override;
  end;

  TJoin = class(TInterfacedObject, ISQLable)

  public
    function ToSQL(AMappingStrategy: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): ICustomCriteria;
  end;

  TFrom = class(TInterfacedObject, ISQLable)
  strict private
    mType: TClass;
    mJoins: TObjectList<TJoin>;
    mQueryBase: ISQLable;
    mArguments: TObjectList<TdormParameter>;
    mAlias: string;
    mWhere: string;
    mGroupBy: string;
    mHaving: string;
    mOrderBy: string;
    mLimit: string;
    mOffset: string;

  strict protected
    function GetTableName(AMappingStrategy: ICacheMappingStrategy;
      AClassObject: TClass): string;

  protected
    FScope: TDictionary<string, TRttiType>;

  public
    constructor Create(AObject: TClass; ASQLable: ISQLable;
      AScope: TDictionary<string, TRttiType>);
    destructor Destroy; override;
    function ToSQL(AMappingStrategy: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): ICustomCriteria;
    //
    function _as(AAlias: string): TFrom;
    function Where(AWhere: string): TFrom; overload;
    function Where(AWhere: string; AParams: array of const): TFrom; overload;
    function groupBy(AGroupBy: string): TFrom;
    function having(AHaving: string): TFrom;
    function limit(ALimit: Integer): TFrom;
    function orderBy(AOrderBy: string): TFrom;

  end;

  TDSQLParser = class sealed
    class function GetColumnByField(AClassName, AAttributeName: string;
      AScope: TDictionary<string, TRttiType>;
      AMapping: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): string;
    class function GetValueOf(AMapping: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy; AArguments: TObjectList<TdormParameter>;
      AIndex: Integer): string;
    class function Parse(ASQLWithParams: string;
      AMapping: ICacheMappingStrategy; AStrategy: IdormPersistStrategy;
      AScope: TDictionary<string, TRttiType>;
      AArguments: TObjectList<TdormParameter>): string;
  end;

  TSQLCustomCriteria = class(TdormCriteria, ICustomCriteria)
  strict protected
    FSQL: string;
    FParameters: TObjectList<TdormParameter>;

  public
    constructor Create(ASQL: string);
    function GetItemClassInfo: PTypeInfo;
    function GetSQL: string;
  end;

function Select(): TSelect;

implementation

uses
  System.SysUtils,
  dorm.Utils,
  dorm.Mappings;

function Select(): TSelect;
begin
  Result := TSelect.Create;
end;

{ TSelect }

destructor TSelect.Destroy;
begin
  FScope.Free;
  inherited;
end;

function TSelect.Distinct(): TSelect;
begin
  FDistinct := true;
  FAll := false;
  Result := Self;
end;

function TSelect.From(AEntity: TClass): TFrom;
begin
  Result := TFrom.Create(AEntity, Self, FScope);
end;

function TSelect.All(): TSelect;
begin
  FDistinct := false;
  FAll := true;
  Result := Self;
end;

constructor TSelect.Create;
begin
  inherited;
  FDistinct := false;
  FAll := false;
  FScope := TDictionary<string, TRttiType>.Create;
end;

function TSelect.ToSQL(AMappingStrategy: ICacheMappingStrategy;
  AStrategy: IdormPersistStrategy)
  : ICustomCriteria;

var
  sql: TStringBuilder;
begin
  sql := TStringBuilder.Create;
  try
    sql.append('SELECT ');
    if (FDistinct) then
      sql.append('DISTINCT ')
    else if (FAll) then
      sql.append('ALL ');

    sql.append('* ');

    // if (mColumns ! = null & & mColumns.length > 0) {
    // sql.append(TextUtils.join(", ", mColumns) + " ");
    // }
    // else {
    // sql.append("* ");
    // }
    Result := TSQLCustomCriteria.Create(sql.toString());
  finally
    sql.Free;
  end;
end;

{ TFrom }

constructor TFrom.Create(AObject: TClass; ASQLable: ISQLable;
  AScope: TDictionary<string, TRttiType>);
begin
  inherited Create;
  mType := AObject;
  FScope := AScope;
  mJoins := TObjectList<TJoin>.Create(true);
  mQueryBase := ASQLable;
  mArguments := TObjectList<TdormParameter>.Create(true);
  FScope.Add(mType.ClassName, TdormUtils.ctx.GetType(mType));
end;

function TFrom.ToSQL(AMappingStrategy: ICacheMappingStrategy;
  AStrategy: IdormPersistStrategy): ICustomCriteria;

var
  sql: TStringBuilder;
  join: TJoin;
  ASQL: string;
begin
  sql := TStringBuilder.Create;
  try
    sql.append(mQueryBase.ToSQL(AMappingStrategy, AStrategy).GetSQL);
    sql.append('FROM ' + GetTableName(AMappingStrategy, mType) + ' ');

    if (mAlias <> '') then
      sql.append(' AS ' + mAlias + ' ');

    for join in mJoins do
    begin
      sql.append(join.ToSQL(AMappingStrategy, AStrategy).GetSQL);
      { TODO -oDaniele -cGeneral : add into scope the classes involved in the join }
    end;

    if mWhere <> '' then
      sql.append('WHERE ' + mWhere + ' ');

    if mGroupBy <> '' then
      sql.append('GROUP BY ' + mGroupBy + ' ');

    if mHaving <> '' then
      sql.append('HAVING ' + mHaving + ' ');

    if mOrderBy <> '' then
      sql.append('ORDER BY ' + mOrderBy + ' ');

    if mLimit <> '' then
      sql.append('ROWS ' + mLimit + ' ');

    ASQL := TDSQLParser.Parse(Trim(sql.toString), AMappingStrategy, AStrategy,
      FScope, mArguments);

    Result := TSQLCustomCriteria.Create(ASQL);
  finally
    sql.Free;
  end;
end;

function TFrom.Where(AWhere: string; AParams: array of const): TFrom;
var
  v: TVarRec;
  S: String;
begin
  mWhere := AWhere;
  for v in AParams do
  begin
    case v.VType of
      vtInteger:
        begin
          mArguments.Add(TdormParameter.Create(TypeInteger, v.VInteger));
        end;
      vtInt64:
        begin
          mArguments.Add(TdormParameter.Create(TypeInteger, v.VInt64^));
        end;
      vtBoolean:
        begin
          mArguments.Add(TdormParameter.Create(TypeBoolean, v.VBoolean));
        end;

      vtExtended:
        begin
          mArguments.Add(TdormParameter.Create(TypeFloat, v.VExtended^));
        end;

      vtString:
        begin
          mArguments.Add(TdormParameter.Create(TypeString, v.VString^));
        end;

      vtWideString:
        begin
          mArguments.Add(TdormParameter.Create(TypeString, String(v.VWideString^)));
        end;

      vtWideChar:
        begin
          mArguments.Add(TdormParameter.Create(TypeString, v.VWideChar));
        end;

      vtChar:
        begin
          mArguments.Add(TdormParameter.Create(TypeString, v.VChar));
        end;

      vtAnsiString:
        begin
          mArguments.Add(TdormParameter.Create(TypeString,
            AnsiString(v.VAnsiString^)));
        end;

      vtUnicodeString:
        begin
          mArguments.Add(TdormParameter.Create(TypeString,
            PChar(v.VUnicodeString)));
        end;

      vtCurrency:
        begin
          mArguments.Add(TdormParameter.Create(TypeDecimal, v.VCurrency^));
        end;
    else
      raise EdormException.Create('Invalid type for argument');
    end;
  end;
  Result := Self;
end;

function TFrom._as(AAlias: string): TFrom;
begin
  mAlias := AAlias;
  Result := Self;
end;

function TFrom.Where(AWhere: string): TFrom;
begin
  mWhere := AWhere;
  mArguments.Clear();
  Result := Self;
end;

destructor TFrom.Destroy;
begin
  mJoins.Free;
  mArguments.Free;
  inherited;
end;

function TFrom.GetTableName(AMappingStrategy: ICacheMappingStrategy;
  AClassObject: TClass): string;
var
  m: TMappingTable;
begin
  m := AMappingStrategy.GetMapping(TdormUtils.ctx.GetType
    (AClassObject));
  if not assigned(m) then
    raise EdormException.Create('Cannot find mapping for ' +
      AClassObject.QualifiedClassName);
  Result := m.TableName;
end;

function TFrom.groupBy(AGroupBy: string): TFrom;
begin
  mGroupBy := AGroupBy;
  Result := Self;
end;

function TFrom.having(AHaving: string): TFrom;
begin
  mHaving := AHaving;
  Result := Self;
end;

function TFrom.orderBy(AOrderBy: string): TFrom;
begin
  mOrderBy := AOrderBy;
  Result := Self;
end;

function TFrom.limit(ALimit: Integer): TFrom;
begin
  mLimit := inttostr(ALimit);
  Result := Self;
end;

{ TJoin }

function TJoin.ToSQL(AMappingStrategy: ICacheMappingStrategy;
  AStrategy: IdormPersistStrategy)
  : ICustomCriteria;
begin

end;

{ TSQLCustomCriteria }

constructor TSQLCustomCriteria.Create(ASQL: string);
begin
  inherited Create;
  FSQL := ASQL;
end;

function TSQLCustomCriteria.GetItemClassInfo: PTypeInfo;
begin
  Result := nil;
end;

function TSQLCustomCriteria.GetSQL: string;
begin
  Result := FSQL;
end;

{ TdormParameter }

function TdormParameter.AsString(AMapping: ICacheMappingStrategy;
  AStrategy: IdormPersistStrategy): string;
begin
  case Self.FParameterType of
    TypeString:
      Result := '''' + AStrategy.EscapeString(FValue.AsString) + '''';
    TypeInteger:
      Result := inttostr(FValue.AsInteger);
    TypeFloat:
      Result := FormatFloat('0.######', FValue.AsExtended);
    TypeBoolean:
      Result := BoolToStr(FValue.AsBoolean, true);
    TypeDate,
      TypeDataTime,
      TypeDecimal:
      Result := 'NOT SUPPORTED';
  end;
end;

constructor TdormParameter.Create(AParameterType: TdormParameterType;
  AValue: TValue);
begin
  inherited Create;
  FParameterType := AParameterType;
  FValue := AValue;
end;

procedure TdormParameter.SetStrategy(const Value: IdormPersistStrategy);
begin
  FStrategy := Value;
end;

procedure TdormParameter.SetValue(const Value: TValue);
begin
  FValue := Value;
end;

{ TDSQLParser }

class function TDSQLParser.GetColumnByField(AClassName, AAttributeName: string;
  AScope: TDictionary<string, TRttiType>;
  AMapping: ICacheMappingStrategy;
  AStrategy: IdormPersistStrategy): string;
var
  m: TMappingTable;
  mf: TMappingField;
  t: TRttiType;
begin
  t := AScope[AClassName];
  if not assigned(t) then
    raise EdormException.CreateFmt('Invalid class name %s', [AClassName]);

  m := AMapping.GetMapping(t);
  if not assigned(m) then
    raise EdormException.CreateFmt('Cannot find mapping for %s',
      [t.QualifiedName]);

  mf := m.FindByName(AAttributeName);
  if not assigned(mf) then
    raise EdormException.CreateFmt('Cannot find attribute %s.%s',
      [AClassName, AAttributeName]);

  Result := mf.FieldName;
end;

class function TDSQLParser.GetValueOf(AMapping: ICacheMappingStrategy;
  AStrategy: IdormPersistStrategy; AArguments: TObjectList<TdormParameter>;
  AIndex: Integer): string;
begin
  Result := AArguments[AIndex].AsString(AMapping, AStrategy);
end;

class function TDSQLParser.Parse(ASQLWithParams: string;
  AMapping: ICacheMappingStrategy; AStrategy: IdormPersistStrategy;
  AScope: TDictionary<string, TRttiType>;
  AArguments: TObjectList<TdormParameter>): string;
var
  sb: TStringBuilder;
  i: Integer;
  c: char;
  state: Integer;
  _ClassName, _AttributeName: string;
  param_index: Integer;
  l: Integer;
const
  SINK = 0;
  SHARP = 1;
  DOT = 6;
  ATTRIBUTE = 2;
  ClassName = 3;
  // QUESTION_MARK = 3;
  SQUOTE = 4;
  DQUOTE = 5;
begin
  sb := TStringBuilder.Create;
  try
    param_index := 0;
    i := 1;
    l := Length(ASQLWithParams);
    state := SINK;
    while i <= l do
    begin
      c := ASQLWithParams[i];
      case state of
        SINK:
          begin
            if c = '#' then
              state := SHARP
            else if c = '?' then
            begin
              sb.append(GetValueOf(AMapping, AStrategy, AArguments,
                param_index));
              inc(param_index);
            end
            else if c = '''' then
            begin
              state := SQUOTE;
              sb.append(c);
            end
            else if c = '"' then
            begin
              state := DQUOTE;
              sb.append(c);
            end
            else
              sb.append(c);
          end;

        SQUOTE:
          begin
            if c = '''' then
              state := SINK;
            sb.append(c);
          end;

        DQUOTE:
          begin
            if c = '"' then
              state := SINK;
            sb.append(c);
          end;

        ATTRIBUTE:
          begin
            if CharInSet(c, ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_']) then
            begin
              _AttributeName := _AttributeName + c;
            end
            else if c = '#' then
            begin
              sb.append(GetColumnByField(_ClassName, _AttributeName, AScope,
                AMapping,
                AStrategy));
              state := SINK;
            end
            else
              raise EdormException.CreateFmt
                ('Invalid character at pos %d. Expected attribute.', [i]);
          end;

        DOT:
          begin
            if CharInSet(c, ['A' .. 'Z', 'a' .. 'z', '_']) then
            begin
              _AttributeName := c;
              state := ATTRIBUTE;
            end
            else
              raise EdormException.CreateFmt
                ('Expected attribute name at pos %d', [i]);
          end;

        ClassName:
          begin
            if CharInSet(c, ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_']) then
              _ClassName := _ClassName + c
            else if c = '.' then
            begin
              state := DOT;
            end;
          end;

        SHARP:
          begin
            if CharInSet(c, ['A' .. 'Z', 'a' .. 'z', '_']) then
            begin
              state := ClassName;
              _ClassName := c;
              _AttributeName := '';
            end
            else
              raise EdormException.CreateFmt
                ('Expected attribute name at position %d', [i])
          end;
      end; // case
      inc(i);
    end; // while

    Result := sb.toString;
  finally
    sb.Free;
  end;
end;

end.
