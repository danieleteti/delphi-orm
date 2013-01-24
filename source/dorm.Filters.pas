unit dorm.Filters;

interface

uses
  Rtti,
  TypInfo,
  Generics.Collections;

type

  TdormCompareOperator = (coEqual, coGreaterThan, coLowerThan, coGreaterOrEqual,
    coLowerOrEqual, coNotEqual, coLike);
  TdormLogicRelation = (lrAnd, lrOr);

  ICriteriaItem = interface;

  ICriteriaItem = interface(IInterface)
    ['{3329869B-79A1-4194-95B1-DEE40696EE80}']
    procedure SetAttribute(const Value: string);
    procedure SetCompareOperator(const Value: TdormCompareOperator);
    procedure SetValue(const Value: TValue);
    procedure SetLogicRelation(const Value: TdormLogicRelation);
    function GetLogicRelation: TdormLogicRelation;
    function GetCompareOperator: TdormCompareOperator;
    function GetValue: TValue;
    function GetAttribute: String;
  end;

  ICriteria = interface(ICriteriaItem)
    ['{3329869B-79A1-4194-95B1-DEE40696EE80}']
    procedure Clear;
    function _Or(Criteria: ICriteria): ICriteria; overload;
    function _And(Criteria: ICriteria): ICriteria; overload;
    function _Or(const Attribute: string; CompareOperator: TdormCompareOperator; Value: TValue)
      : ICriteria; overload;
    function _And(const Attribute: string; CompareOperator: TdormCompareOperator; Value: TValue)
      : ICriteria; overload;
    function Count: Integer;
    function GetCriteria(const index: Integer): ICriteria;
  end;

  ICustomCriteria = interface(ICriteria)
    ['{7F19727A-8113-43F0-8211-A3EFB47E57EB}']
    function GetSQL: string;
    function GetItemClassInfo: PTypeInfo;
  end;

  TdormCriteria = class(TInterfacedObject, ICriteria)
  private
    FAttribute: String;
    FLogicRelation: TdormLogicRelation;
    FValue: TValue;
    FCompareOperator: TdormCompareOperator;
    FItems: TList<ICriteria>;
    function Add(Criteria: ICriteria; LogicRelation: TdormLogicRelation)
      : ICriteria; overload;
    function Add(const Attribute: string; CompareOperator: TdormCompareOperator; Value: TValue;
      LogicRelation: TdormLogicRelation = lrAnd): ICriteria; overload;
  public
    constructor Create; overload; virtual;
    constructor Create(const Attribute: string; CompareOperator: TdormCompareOperator;
      Value: TValue); overload; virtual;
    constructor Create(Criteria: ICriteria); overload; virtual;
    destructor Destroy; override;
    { ICriteria }
    procedure Clear;
    function _Or(Criteria: ICriteria): ICriteria; overload;
    function _And(Criteria: ICriteria): ICriteria; overload;
    function _Or(const Attribute: string; CompareOperator: TdormCompareOperator; Value: TValue)
      : ICriteria; overload;
    function _And(const Attribute: string; CompareOperator: TdormCompareOperator; Value: TValue)
      : ICriteria; overload;
    function Count: Integer;
    function GetCriteria(const index: Integer): ICriteria;
    class function NewCriteria(const Attribute: string; CompareOperator: TdormCompareOperator;
      Value: TValue): ICriteria; overload;
    class function NewCriteria(Criteria: ICriteria): ICriteria; overload;
    { ICriteriaItem }
    procedure SetAttribute(const Value: string);
    procedure SetCompareOperator(const Value: TdormCompareOperator);
    procedure SetValue(const Value: TValue);
    procedure SetLogicRelation(const Value: TdormLogicRelation);
    function GetLogicRelation: TdormLogicRelation;
    function GetCompareOperator: TdormCompareOperator;
    function GetValue: TValue;
    function GetAttribute: string;

  end;

  TdormCriteriaItem = class(TdormCriteria, ICriteria, ICriteriaItem)
  private
    FCompareOperator: TdormCompareOperator;
    FAttribute: string;
    FValue: TValue;
    FLogicRelation: TdormLogicRelation;
  protected
    procedure SetAttribute(const Value: string);
    procedure SetCompareOperator(const Value: TdormCompareOperator);
    procedure SetValue(const Value: TValue);
    procedure SetLogicRelation(const Value: TdormLogicRelation);
    function GetLogicRelation: TdormLogicRelation;
    function GetAttribute: string;
    function GetCompareOperator: TdormCompareOperator;
    function GetValue: TValue;
  public
    constructor Create(const Attribute: string; CompareOperator: TdormCompareOperator;
      Value: TValue); override;
    property Attribute: string read FAttribute write SetAttribute;
    property CompareOperator: TdormCompareOperator read FCompareOperator
      write SetCompareOperator;
    property Value: TValue read FValue write SetValue;
    property LogicRelation: TdormLogicRelation read FLogicRelation
      write SetLogicRelation;
  end;

  TSimpleFinder = class(TdormCriteria, ICustomCriteria)
  protected
    FSQL: string;
    FItemClassInfo: PTypeInfo;
  public
    constructor Create(AItemClassInfo: PTypeInfo; ASQL: string); virtual;
    function GetSQL: string;
    function GetItemClassInfo: PTypeInfo;
  end;

function NewCriteria(Criteria: ICriteria): ICriteria; overload;
function NewCriteria(const Attribute: string; CompareOperator: TdormCompareOperator;
  Value: TValue)
  : ICriteria; overload;

implementation

uses
  dorm.Commons,
  dorm.Utils;

function NewCriteria(Criteria: ICriteria): ICriteria;
begin
  Result := TdormCriteria.NewCriteria(Criteria);
end;

function NewCriteria(const Attribute: string; CompareOperator: TdormCompareOperator;
  Value: TValue): ICriteria;
begin
  Result := TdormCriteria.NewCriteria(Attribute, CompareOperator, Value);
end;

{ TdormSimpleSearchCriteria }

constructor TSimpleFinder.Create(AItemClassInfo: PTypeInfo;
  ASQL: string);
begin
  inherited Create;
  FSQL := ASQL;
  FItemClassInfo := AItemClassInfo;
end;

function TSimpleFinder.GetItemClassInfo: PTypeInfo;
begin
  Result := FItemClassInfo;
end;

function TSimpleFinder.GetSQL: string;
begin
  Result := FSQL;
end;

{ TdormCriteria }

{ TdormCriteria }

function TdormCriteria.Add(const Attribute: string; CompareOperator: TdormCompareOperator;
  Value: TValue; LogicRelation: TdormLogicRelation): ICriteria;

var
  Item: TdormCriteriaItem;
begin
  Item := TdormCriteriaItem.Create;
  Item.Attribute := Attribute;
  Item.CompareOperator := CompareOperator;
  Item.Value := Value;
  Item.LogicRelation := LogicRelation;
  FItems.Add(Item);
  Result := Self;
end;

function TdormCriteria.Add(Criteria: ICriteria; LogicRelation: TdormLogicRelation)
  : ICriteria;

var
  Item: ICriteria;
begin
  Item := Criteria;
  Item.SetLogicRelation(LogicRelation);
  FItems.Add(Item);
  Result := Self;
end;

procedure TdormCriteria.Clear;
begin
  FItems.Clear;
end;

function TdormCriteria.Count: Integer;
begin
  Result := FItems.Count;
end;

constructor TdormCriteria.Create(Criteria: ICriteria);
begin
  Create;
  Add(Criteria, lrAnd);
end;

constructor TdormCriteria.Create(const Attribute: string; CompareOperator: TdormCompareOperator;
  Value: TValue);
begin
  Create;
  Add(Attribute, CompareOperator, Value);
end;

constructor TdormCriteria.Create;
begin
  inherited;
  FItems := TList<ICriteria>.Create;
end;

destructor TdormCriteria.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TdormCriteria.GetAttribute: string;
begin
  Result := FAttribute;
end;

function TdormCriteria.GetCompareOperator: TdormCompareOperator;
begin
  Result := FCompareOperator;
end;

function TdormCriteria.GetCriteria(const index: Integer): ICriteria;
begin
  Result := FItems[index];
end;

function TdormCriteria.GetLogicRelation: TdormLogicRelation;
begin
  Result := FLogicRelation;
end;

function TdormCriteria.GetValue: TValue;
begin
  Result := FValue;
end;

class function TdormCriteria.NewCriteria(Criteria: ICriteria): ICriteria;
begin
  Result := TdormCriteria.Create;
  Result._And(Criteria);
end;

procedure TdormCriteria.SetAttribute(const Value: string);
begin
  FAttribute := Value;
end;

procedure TdormCriteria.SetCompareOperator(const Value: TdormCompareOperator);
begin
  FCompareOperator := Value;
end;

procedure TdormCriteria.SetLogicRelation(const Value: TdormLogicRelation);
begin
  FLogicRelation := Value;
end;

procedure TdormCriteria.SetValue(const Value: TValue);
begin
  FValue := Value;
end;

function TdormCriteria._And(Criteria: ICriteria): ICriteria;
begin
  Result := Add(Criteria, lrAnd);
end;

function TdormCriteria._Or(Criteria: ICriteria): ICriteria;
begin
  Result := Add(Criteria, lrOr);
end;

class function TdormCriteria.NewCriteria(const Attribute: string;
  CompareOperator: TdormCompareOperator; Value: TValue): ICriteria;
begin
  Result := TdormCriteria.Create;
  Result._And(Attribute, CompareOperator, Value);
end;

function TdormCriteria._And(const Attribute: string; CompareOperator: TdormCompareOperator;
  Value: TValue): ICriteria;
begin
  Result := Add(Attribute, CompareOperator, Value, lrAnd);
end;

function TdormCriteria._Or(const Attribute: string; CompareOperator: TdormCompareOperator;
  Value: TValue): ICriteria;
begin
  Result := Add(Attribute, CompareOperator, Value, lrOr);
end;

{ TdormSearch }

constructor TdormCriteriaItem.Create(const Attribute: string; CompareOperator: TdormCompareOperator;
  Value: TValue);
begin
  Create;
  FAttribute := Attribute;
  FCompareOperator := CompareOperator;
  FValue := Value;
end;

function TdormCriteriaItem.GetAttribute: string;
begin
  Result := FAttribute;
end;

function TdormCriteriaItem.GetCompareOperator: TdormCompareOperator;
begin
  Result := FCompareOperator;
end;

function TdormCriteriaItem.GetLogicRelation: TdormLogicRelation;
begin
  Result := FLogicRelation;
end;

function TdormCriteriaItem.GetValue: TValue;
begin
  Result := FValue;
end;

procedure TdormCriteriaItem.SetAttribute(const Value: string);
begin
  FAttribute := Value;
end;

procedure TdormCriteriaItem.SetCompareOperator(const Value
  : TdormCompareOperator);
begin
  FCompareOperator := Value;
end;

procedure TdormCriteriaItem.SetLogicRelation(const Value: TdormLogicRelation);
begin
  FLogicRelation := Value;
end;

procedure TdormCriteriaItem.SetValue(const Value: TValue);
begin
  FValue := Value;
end;

end.
