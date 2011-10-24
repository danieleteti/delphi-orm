unit dorm.DBCreator.Interbase;

interface

uses
  dorm.DBCreator,
  dorm.Commons;

type
  TdormInterbaseDBCreator = class(TdormDBCreator)
  protected
    function GetDatabaseFieldDeclaration(const dormFieldMapping
      : TdormFieldMapping): string; override;
  public
    procedure Execute; override;
  end;

implementation

uses
  SysUtils, Generics.Collections, System.Classes;

{ TdormInterbaseDBCreator }
procedure TdormInterbaseDBCreator.Execute;
var
  Tables: TList<string>;
  Table: string;
  TableMapping: TArray<TdormFieldMapping>;
  FieldMapping: TdormFieldMapping;
  TableName: string;
  s: string;
  Indexes, Uniques: TStringList;
  index_name: string;
  ShortTableName: string;
  short_index_name: string;
  IndexesNames: TStringList;
  idx_count: Integer;
begin
  Indexes := TStringList.Create;
  try
    Uniques := TStringList.Create;
    try
      IndexesNames := TStringList.Create;
      try
        FSQL.Clear;
        Tables := FSession.GetPersistentClassesName;
        try
          for Table in Tables do
          begin
            FSession.GetLogger.Debug(Table);
            TableName := FSession.GetTableName(Table);
            TableMapping := FSession.GetTableMapping(Table);
            FSQL.Add('DROP GENERATOR SEQ_' + TableName + '_ID;');
            FSQL.Add('CREATE GENERATOR SEQ_' + TableName + '_ID;');
            FSQL.Add('SET GENERATOR SEQ_' + TableName + '_ID TO 0;');
            FSQL.Add('DROP TABLE ' + TableName + ';');
            FSQL.Add('CREATE TABLE ' + TableName + '(');
            // Interbase SPECIFIC!!!!!
            s := '  ';
            Indexes.Clear;
            Uniques.Clear;
            for FieldMapping in TableMapping do
            begin
              s := s + GetDatabaseFieldDeclaration(FieldMapping) + ', ';
              if FieldMapping.index_type = itUnique then
                Uniques.Add(FieldMapping.field);
              if FieldMapping.index_type = itIndex then
                Indexes.Add(FieldMapping.field);
            end;
            Delete(s, Length(s) - 1, 2);
            FSQL.Add(s + ');');
            FSQL.Add('DELETE FROM ' + TableName + ';');

            ShortTableName := Copy(TableName, 1, 8);
            for index_name in Indexes do
            begin
              short_index_name := 'IDX_' + ShortTableName + '_' +
                Copy(index_name, 1, 8);
              idx_count := 0;
              while IndexesNames.IndexOf(short_index_name) > -1 do
              begin
                Inc(idx_count);
                short_index_name := short_index_name + inttostr(idx_count);
              end;
              IndexesNames.Add(short_index_name);
              FSQL.Add('CREATE INDEX ' + short_index_name + ' on ' + TableName +
                '(' + index_name + ');');
            end;

            for index_name in Uniques do
            begin
              short_index_name := 'IDX_' + ShortTableName + '_' +
                Copy(index_name, 1, 8);
              idx_count := 0;
              while IndexesNames.IndexOf(short_index_name) > -1 do
              begin
                Inc(idx_count);
                short_index_name := short_index_name + inttostr(idx_count);
              end;
              FSQL.Add('CREATE UNIQUE INDEX ' + short_index_name + ' on ' +
                TableName + '(' + index_name + ');');
            end;
          end;
          FSession.GetLogger.Debug(FSQL.Text);
        finally
          Tables.Free;
        end;
      finally
        IndexesNames.Free;
      end;
    finally
      Uniques.Free;
    end;
  finally
    Indexes.Free;
  end;
end;

function TdormInterbaseDBCreator.GetDatabaseFieldDeclaration
  (const dormFieldMapping: TdormFieldMapping): string;
begin
  Result := dormFieldMapping.field;
  if SameText(dormFieldMapping.field_type, 'integer') then
    Result := Result + ' INTEGER'
  else if SameText(dormFieldMapping.field_type, 'string') then
    Result := Result + ' VARCHAR(' + inttostr(dormFieldMapping.size) + ')'
  else if SameText(dormFieldMapping.field_type, 'date') then
    Result := Result + ' DATE'
  else if SameText(dormFieldMapping.field_type, 'datetime') then
    Result := Result + ' TIMESTAMP'
  else if SameText(dormFieldMapping.field_type, 'decimal') then
    Result := Result + Format(' DECIMAL(%d,%d)',
      [dormFieldMapping.size, dormFieldMapping.precision])
  else if SameText(dormFieldMapping.field_type, 'blob') then
    Result := Result + ' BLOB SUB_TYPE 0 SEGMENT SIZE 16384'
  else if SameText(dormFieldMapping.field_type, 'text') then
    Result := Result + ' BLOB SUB_TYPE 1 SEGMENT SIZE 16384'
  else if SameText(dormFieldMapping.field_type, 'boolean') then
    Result := Result + ' SMALLINT';
  if dormFieldMapping.pk then
    Result := Result + ' not null primary key';
end;

initialization

TdormDBCreatorsRegister.Register(TdormInterbaseDBCreator, 'Interbase');

end.
