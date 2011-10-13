unit dorm.DBCreator.Firebird;

interface

uses
  dorm.DBCreator,
  dorm.Commons;

type
  TdormFirebirdDBCreator = class(TdormDBCreator)
    function GetDatabaseFieldDeclaration(const dormFieldMapping: TdormFieldMapping): string; override;
  end;

implementation

uses
  SysUtils;

{ TdormFirebirdDBCreator }

function TdormFirebirdDBCreator.GetDatabaseFieldDeclaration(const dormFieldMapping: TdormFieldMapping): string;
begin
  Result := dormFieldMapping.field;
  if SameText(dormFieldMapping.field_type, 'integer') then
    Result := Result + ' BIGINT'
  else if SameText(dormFieldMapping.field_type, 'string') then
    Result := Result + ' VARCHAR(' + inttostr(dormFieldMapping.size) + ')'
  else if SameText(dormFieldMapping.field_type, 'date') then
    Result := Result + ' DATE'
  else if SameText(dormFieldMapping.field_type, 'datetime') then
    Result := Result + ' TIMESTAMP'
  else if SameText(dormFieldMapping.field_type, 'decimal') then
    Result := Result + Format(' DECIMAL(%d,%d)', [dormFieldMapping.size, dormFieldMapping.precision])
  else if SameText(dormFieldMapping.field_type, 'blob') then
    Result := Result + ' BLOB SUB_TYPE 0 SEGMENT SIZE 16384'
  else if SameText(dormFieldMapping.field_type, 'text') then
    Result := Result + ' BLOB SUB_TYPE 1 SEGMENT SIZE 16384'
  else if SameText(dormFieldMapping.field_type, 'boolean') then
    Result := Result + ' SMALLINT';
  if dormFieldMapping.pk then
    Result := Result + ' primary key';
end;

end.
