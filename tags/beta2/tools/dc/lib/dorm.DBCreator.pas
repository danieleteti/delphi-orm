unit dorm.DBCreator;

interface

uses
  dorm,
  Classes,
  SysUtils,
  superobject,
  dorm.Commons;

type
  TdormDBCreator = class abstract
  private
    FSession: TSession;
    FMapping: ISuperObject;
    FSQL: TStringList;
  protected
    function GetDatabaseFieldDeclaration(const dormFieldMapping: TdormFieldMapping): string; virtual; abstract;
  public
    constructor Create(Session: dorm.TSession);
    procedure Execute;
    function GetSQLScript: TStringList;
    destructor Destroy; override;
  end;

implementation

uses
  Generics.Collections,
  dorm.adapter.Firebird.Factory;

{ TdormDBCreator }

constructor TdormDBCreator.Create(Session: dorm.TSession);
begin
  inherited Create;
  FSession := Session;
  FMapping := FSession.GetMapping;
  FSQL := TStringList.Create;
end;

destructor TdormDBCreator.Destroy;
begin
  FSession.Free;
  inherited;
end;

procedure TdormDBCreator.Execute;
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
            FSQL.Add('DROP SEQUENCE SEQ_' + TableName + '_ID;');
            FSQL.Add('CREATE SEQUENCE SEQ_' + TableName + '_ID;');
            FSQL.Add('ALTER SEQUENCE SEQ_' + TableName + '_ID RESTART WITH 0;');
            FSQL.Add('RECREATE TABLE ' + TableName + '('); // FIREBIRD SPECIFIC!!!!!
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
              short_index_name := 'IDX_' + ShortTableName + '_' + Copy(index_name, 1, 8);
              idx_count := 0;
              while IndexesNames.IndexOf(short_index_name) > -1 do
              begin
                Inc(idx_count);
                short_index_name := short_index_name + inttostr(idx_count);
              end;
              IndexesNames.Add(short_index_name);
              FSQL.Add('CREATE INDEX ' + short_index_name + ' on ' + TableName + '(' + index_name + ');');
            end;

            for index_name in Uniques do
            begin
              short_index_name := 'IDX_' + ShortTableName + '_' + Copy(index_name, 1, 8);
              idx_count := 0;
              while IndexesNames.IndexOf(short_index_name) > -1 do
              begin
                Inc(idx_count);
                short_index_name := short_index_name + inttostr(idx_count);
              end;
              FSQL.Add('CREATE UNIQUE INDEX ' + short_index_name + ' on ' + TableName + '(' +
                index_name + ');');
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

function TdormDBCreator.GetSQLScript: TStringList;
begin
  Result := FSQL;
end;

end.
