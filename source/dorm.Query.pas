unit dorm.Query;

interface

uses Generics.Collections, dorm.Mappings.Strategies, System.Rtti, dorm.Filters,
  dorm.Commons, System.TypInfo;

type
  TdormParameterType = (TypeString, TypeInteger, TypeFloat, TypeBoolean,
    TypeDate, TypeDataTime, TypeDecimal);

  TdormParameter = class
  private
    FAsString: String;
    FStrategy: IdormPersistStrategy;
    procedure SetStrategy(const Value: IdormPersistStrategy);
    procedure SetValue(const Value: TValue);

  strict protected
    FParameterType: TdormParameterType;
    FValue: TValue;

  public
    constructor Create(AParameterType: TdormParameterType; AValue: TValue);
    function AsString(AMapping: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): String;
    property Strategy: IdormPersistStrategy read FStrategy write SetStrategy;
  end;

  ISQLable = interface
    ['{63C9E9C1-B023-4550-B848-ACD63F2B8051}']
    function ToSQL(AMappingStrategy: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): ICustomCriteria;
  end;

  TFrom = class;

  TSelect = class(TInterfacedObject, ISQLable)
  private
    FDistinct: Boolean;
    FAll: Boolean;
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
    mAlias: String;
    mWhere: String;
    mGroupBy: String;
    mHaving: String;
    mOrderBy: String;
    mLimit: String;
    mOffset: String;

  strict protected
    function GetTableName(AMappingStrategy: ICacheMappingStrategy;
      AClassObject: TClass): String;

  protected
    FScope: TDictionary<string, TRttiType>;

  public
    constructor Create(AObject: TClass; ASQLable: ISQLable;
      AScope: TDictionary<string, TRttiType>);
    destructor Destroy; override;
    function ToSQL(AMappingStrategy: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): ICustomCriteria;
    //
    function _as(AAlias: String): TFrom;
    function Where(AWhere: String): TFrom; overload;
    function Where(AWhere: String; AParams: array of const): TFrom; overload;
    function groupBy(AGroupBy: String): TFrom;
    function having(AHaving: String): TFrom;
    function limit(ALimit: Integer): TFrom;
    function orderBy(AOrderBy: String): TFrom;

  end;

  TDSQLParser = class sealed
    class function GetColumnByField(AClassName, AAttributeName: String;
      AScope: TDictionary<string, TRttiType>;
      AMapping: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy): String;
    class function GetValueOf(AMapping: ICacheMappingStrategy;
      AStrategy: IdormPersistStrategy; AArguments: TObjectList<TdormParameter>;
      AIndex: Integer): String;
    class function Parse(ASQLWithParams: String;
      AMapping: ICacheMappingStrategy; AStrategy: IdormPersistStrategy;
      AScope: TDictionary<string, TRttiType>;
      AArguments: TObjectList<TdormParameter>): String;
  end;

  TSQLCustomCriteria = class(TdormCriteria, ICustomCriteria)
  strict protected
    FSQL: String;
    FParameters: TObjectList<TdormParameter>;

  public
    constructor Create(ASQL: String);
    function GetItemClassInfo: PTypeInfo;
    function GetSQL: string;
  end;

function Select(): TSelect;

implementation

uses
  System.SysUtils, dorm.Utils, dorm.Mappings;

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
  ASQL: String;
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

function TFrom.Where(AWhere: String; AParams: array of const): TFrom;
var
  v: TVarRec;
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

function TFrom._as(AAlias: String): TFrom;
begin
  mAlias := AAlias;
  Result := Self;
end;

function TFrom.Where(AWhere: String): TFrom;
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
  AClassObject: TClass): String;
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

function TFrom.groupBy(AGroupBy: String): TFrom;
begin
  mGroupBy := AGroupBy;
  Result := Self;
end;

function TFrom.having(AHaving: String): TFrom;
begin
  mHaving := AHaving;
  Result := Self;
end;

function TFrom.orderBy(AOrderBy: String): TFrom;
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

constructor TSQLCustomCriteria.Create(ASQL: String);
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
  AStrategy: IdormPersistStrategy): String;
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

class function TDSQLParser.GetColumnByField(AClassName, AAttributeName: String;
  AScope: TDictionary<string, TRttiType>;
  AMapping: ICacheMappingStrategy;
  AStrategy: IdormPersistStrategy): String;
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
  AIndex: Integer): String;
begin
  Result := AArguments[AIndex].AsString(AMapping, AStrategy);
end;

class function TDSQLParser.Parse(ASQLWithParams: String;
  AMapping: ICacheMappingStrategy; AStrategy: IdormPersistStrategy;
  AScope: TDictionary<string, TRttiType>;
  AArguments: TObjectList<TdormParameter>): String;
var
  sb: TStringBuilder;
  i: Integer;
  c: char;
  state: Integer;
  _ClassName, _AttributeName: String;
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
                ('Invalid character at pos %d', [i]);
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
