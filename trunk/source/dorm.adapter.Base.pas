unit dorm.adapter.Base;

interface

uses
  dorm.Commons,
  dorm.Filters,
  dorm.Mappings;

type
  TBaseAdapter = class abstract(TdormInterfacedObject)
  protected
    function GetWhereSQL(ACriteria: ICriteria; AMappingTable: TMappingTable): string; overload;
    function GetWhereSQL(ACriteriaItem: ICriteriaItem; AMappingTable: TMappingTable)
      : string; overload;
    // function GetFieldMappingByAttribute(AttributeName: string;
    // AMappingTable: TMappingTable): TMappingField;
    function EscapeString(const Value: string): string;
    function EscapeDate(const Value: TDate): string;
    function EscapeDateTime(const Value: TDate): string;
  public
    function GetSelectSQL(Criteria: ICriteria;
      AMappingTable: TMappingTable): string;
      overload; virtual;
    function GetSelectSQL(Criteria: ICustomCriteria): string; overload;
  end;

implementation

uses
  SysUtils;

{ TBaseAdapter }

// function TBaseAdapter.GetFieldMappingByAttribute(AttributeName: string;
// AMappingTable: TMappingTable): TMappingField;
// var
// fm: TMappingField;
// begin
// for fm in AMappingTable.Fields do
// if fm.name = AttributeName then
// Exit(fm);
// raise EdormException.CreateFmt('Unknown field attribute %s', [AttributeName]);
// end;

function TBaseAdapter.GetWhereSQL(ACriteria: ICriteria; AMappingTable: TMappingTable): string;
var
  I: Integer;
  SQL: String;
  CritItem: ICriteriaItem;
  Crit: ICriteria;
begin
  if ACriteria.Count > 0 then
    for I := 0 to ACriteria.Count - 1 do
    begin
      CritItem := ACriteria.GetCriteria(I);
      if I > 0 then
        case CritItem.GetLogicRelation of
          lrAnd:
            SQL := SQL + ' AND ';
          lrOr:
            SQL := SQL + ' OR ';
        end;
      if TInterfacedObject(CritItem).GetInterface(ICriteria, Crit) then
      begin
        if Crit.Count > 0 then
          SQL := SQL + ' (' + GetWhereSQL(Crit, AMappingTable) + ') '
        else
          SQL := SQL + GetWhereSQL(CritItem, AMappingTable);
      end
      else
        SQL := SQL + GetWhereSQL(CritItem, AMappingTable);
    end
  else
  begin
    if ACriteria.GetAttribute = '' then
      SQL := ''
    else
    begin
      CritItem := TdormCriteriaItem.Create(ACriteria.GetAttribute, ACriteria.GetCompareOperator,
        ACriteria.GetValue);
      SQL := GetWhereSQL(CritItem, AMappingTable);
    end;
  end;
  Result := SQL;
end;

function TBaseAdapter.GetSelectSQL(Criteria: ICriteria; AMappingTable: TMappingTable): string;
var
  SQL: string;
  _fields: TMappingFieldList;
  select_fields: string;
  WhereSQL: string;
begin
  _fields := AMappingTable.Fields;
  select_fields := GetSelectFieldsList(_fields, true);
  if Assigned(Criteria) then
  begin
    SQL := 'SELECT ' + select_fields + ' FROM ' + AMappingTable.TableName;
    WhereSQL := GetWhereSQL(Criteria, AMappingTable);
    if WhereSQL <> EmptyStr then
      SQL := SQL + ' WHERE ' + WhereSQL;
  end
  else // Criteria is nil or is empty
    SQL := 'SELECT ' + select_fields + ' FROM ' + AMappingTable.TableName + ' ';
  Result := SQL;
end;

function TBaseAdapter.GetSelectSQL(Criteria: ICustomCriteria): string;
begin
  Result := Criteria.GetSQL;
end;

function TBaseAdapter.GetWhereSQL(ACriteriaItem: ICriteriaItem;
  AMappingTable: TMappingTable): string;
var
  SQL: string;
  fm: TMappingField;
  d: TDate;
  dt: TDateTime;
begin
  fm := AMappingTable.FindByName(ACriteriaItem.GetAttribute);
  if not Assigned(fm) then
    raise EdormException.CreateFmt('Unknown field attribute "%s"."%s"',
      [AMappingTable.TableName, ACriteriaItem.GetAttribute]);
  SQL := fm.FieldName;
  case ACriteriaItem.GetCompareOperator of
    coEqual:
      SQL := SQL + ' = ';
    coGreaterThan:
      SQL := SQL + ' > ';
    coLowerThan:
      SQL := SQL + ' < ';
    coGreaterOrEqual:
      SQL := SQL + ' >= ';
    coLowerOrEqual:
      SQL := SQL + ' <= ';
    coNotEqual:
      SQL := SQL + ' != ';
    coLike:
      SQL := SQL + ' LIKE ';
  end;

  if fm.FieldType = 'string' then
    SQL := SQL + '''' + EscapeString(ACriteriaItem.GetValue.AsString) + ''''
  else if fm.FieldType = 'uniqueidentifier' then
    SQL := SQL + '''' + EscapeString(ACriteriaItem.GetValue.AsString) + ''''
  else if fm.FieldType = 'integer' then
    SQL := SQL + inttostr(ACriteriaItem.GetValue.AsInteger)
  else if fm.FieldType = 'boolean' then
    SQL := SQL + BoolToStr(ACriteriaItem.GetValue.AsBoolean)
  else if fm.FieldType = 'boolean' then
    SQL := SQL + BoolToStr(ACriteriaItem.GetValue.AsBoolean)
  else if fm.FieldType = 'date' then
  begin
    d := ACriteriaItem.GetValue.AsExtended;
    SQL := SQL + '''' + EscapeDate(d) + ''''
  end
  else if (fm.FieldType = 'timestamp') or (fm.FieldType = 'datetime') then
  begin
    dt := ACriteriaItem.GetValue.AsExtended;
    SQL := SQL + '''' + EscapeDateTime(dt) + ''''
  end
  else
    raise EdormException.CreateFmt('Unknown type %s in criteria', [fm.FieldType]);
  Result := SQL;
end;

function TBaseAdapter.EscapeDate(const Value: TDate): string;
begin
  Result := FormatDateTime('YYYY-MM-DD', Value);
end;

function TBaseAdapter.EscapeDateTime(const Value: TDate): string;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS', Value);
end;

function TBaseAdapter.EscapeString(const Value: string): string;
begin
  Result := StringReplace(Value, '''', '''''', [rfReplaceAll]);
end;

end.
