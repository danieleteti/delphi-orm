unit dorm.MappingCreator.Strategy.Firebird;

interface

uses
  System.Classes, dorm.MappingCreator.Strategy, dorm.adapter.UIB.Facade,
  Generics.Collections, Data.DBXJSON, dorm.Configuration,
  superobject; // used only to format json... we should replace it!

type
  TFirebirdMappingCreatorStrategy = class(TMappingCreatorStrategy)
  protected
    FB: TUIBFacade;
    function GetTablesName: TList<string>; override;
    function CreatePKFieldMapping(TableName: String): TJSONObject;
      override;
    function CreateFieldsMapping(TableName: String): TJSONArray;
      override;
    procedure InitUserParameters; override;
    procedure Connect; override;
    procedure Disconnect; override;
    function InternalExecute(AOutputStream: TStream): Boolean; override;
  public
    const
    CONNECTION_STRING = 'connection_string';
    USERNAME = 'username';
    PASSWORD = 'password';
    PACKAGE = 'package';
    constructor Create; override;
    destructor Destroy; override;
  end;

  TInterbaseMappingCreatorStrategy = class(TFirebirdMappingCreatorStrategy)
  protected
    function InternalExecute(AOutputStream: TStream): Boolean; override;
    procedure Connect; override;
  end;

implementation

uses
  System.SysUtils, System.Generics.Collections, UIB;

const
  GET_TABLES_AND_VIEWS = 'select rdb$relation_name NAME ' +
    'from rdb$relations ' +
    'where rdb$system_flag is null or rdb$system_flag = 0 ' +
    'ORDER BY rdb$view_blr, rdb$relation_name';
  GET_TABLE_INFO = ' SELECT r.RDB$FIELD_NAME AS "Field", ' +
    '         CASE f.RDB$FIELD_TYPE' +
    '           WHEN 261 THEN ''BLOB'' ' +
    '           WHEN 14 THEN ''CHAR'' ' +
    '           WHEN 40 THEN ''CSTRING'' ' +
    '           WHEN 11 THEN ''D_FLOAT'' ' +
    '           WHEN 27 THEN ''DOUBLE'' ' +
    '           WHEN 10 THEN ''FLOAT'' ' +
    '           WHEN 16 THEN ''INT64'' ' +
    '           WHEN 8 THEN ''INTEGER'' ' +
    '           WHEN 9 THEN ''QUAD'' ' +
    '           WHEN 7 THEN ''SMALLINT'' ' +
    '           WHEN 12 THEN ''DATE'' ' +
    '           WHEN 13 THEN ''TIME'' ' +
    '           WHEN 35 THEN ''TIMESTAMP'' ' +
    '           WHEN 37 THEN ''VARCHAR'' ' +
    '           ELSE ''UNKNOWN'' ' +
    '         END AS "Type",' +
    '         f.RDB$FIELD_LENGTH AS "Length", ' +
    '         f.RDB$FIELD_PRECISION AS "Precision", ' +
    '         f.RDB$FIELD_SCALE AS "Scale", ' +
    '         MIN(rc.RDB$CONSTRAINT_TYPE) AS "Constraint", ' +
    '         MIN(i.RDB$INDEX_NAME) AS "Idx", ' +
    '         CASE WHEN r.RDB$NULL_FLAG = 1 THEN ''NO'' ELSE ''YES'' END AS "Null", '
    +
    '         r.RDB$DEFAULT_VALUE AS "Default",' +
    '         r.RDB$FIELD_POSITION AS "Pos"' +
    '     FROM RDB$RELATION_FIELDS r' +
    '    LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME' +
    '    LEFT JOIN RDB$INDEX_SEGMENTS s ON s.RDB$FIELD_NAME=r.RDB$FIELD_NAME' +
    '    LEFT JOIN RDB$INDICES i ON i.RDB$INDEX_NAME = s.RDB$INDEX_NAME' +
    '          AND i.RDB$RELATION_NAME=r.RDB$RELATION_NAME' +
    '    LEFT JOIN RDB$RELATION_CONSTRAINTS rc ON rc.RDB$INDEX_NAME = s.RDB$INDEX_NAME'
    +
    '          AND rc.RDB$INDEX_NAME = i.RDB$INDEX_NAME' +
    '          AND rc.RDB$RELATION_NAME = i.RDB$RELATION_NAME' +
    '    LEFT JOIN RDB$REF_CONSTRAINTS refc ON rc.RDB$CONSTRAINT_NAME = refc.RDB$CONSTRAINT_NAME'
    +
    '   WHERE r.RDB$RELATION_NAME=''%s'' ' +
    ' GROUP BY "Field", ' +
    '         "Type", ' +
    '         "Length", ' +
    '         "Precision", ' +
    '         "Scale", ' +
    '         "Null", ' +
    '         "Default", ' +
    '         "Pos" ' +
    'ORDER BY "Pos";';

  { TFirebirdMappingCreatorStrategy }
procedure TFirebirdMappingCreatorStrategy.Connect;
begin
  inherited;
  FreeAndNil(FB);
  FB := TUIBFacade.Create(
    'fbclient.dll',
    GetUserParameterByName(USERNAME),
    GetUserParameterByName(PASSWORD),
    GetUserParameterByName(CONNECTION_STRING)
    );
  FB.GetConnection;
end;

constructor TFirebirdMappingCreatorStrategy.Create;
begin
  inherited;

end;

function TFirebirdMappingCreatorStrategy.CreateFieldsMapping(
  TableName: String): TJSONArray;

  function GetFieldMapping(AQuery: TUIBQuery): TJSONObject;
  var
    RawFieldType: String;
    dormFieldType: string;
    Obj: TJSONObject;
    FieldSize: Integer;
    RawFieldName: string;
  begin
    RawFieldType := Trim(AQuery.Fields.ByNameAsString['Type']);
    RawFieldName := Trim(AQuery.Fields.ByNameAsString['Field']);
    if (RawFieldType = 'INT64') or (RawFieldType = 'INTEGER') or
      (RawFieldType = 'SMALLINT') then
      dormFieldType := 'integer'
    else if (RawFieldType = 'VARCHAR') or (RawFieldType = 'CHAR') then
      dormFieldType := 'string'
    else if (RawFieldType = 'DATE') then
      dormFieldType := 'date'
    else if (RawFieldType = 'TIMESTAMP') then
      dormFieldType := 'datetime'
    else if (RawFieldType = 'BLOB') then
      dormFieldType := 'blob'
    else
    begin
      raise Exception.Create('Cannot map field ' + TableName + '.' +
        AQuery.Fields.ByNameAsString['Field'] + ' of type ' + RawFieldType);
    end;

    // {"name":"Age", "field":"AGE", "field_type":"integer"},
    // {"name":"FirstName", "field":"FIRST_NAME", "field_type":"string", "size": 50},

    Obj := TJSONObject.Create;
    Obj
      .AddPair('name',
      FieldNameToClassAtribute(RawFieldName))
      .AddPair('field', Trim(AQuery.Fields.ByNameAsString['Field']))
      .AddPair('field_type', dormFieldType);
    if dormFieldType = 'string' then
      Obj.AddPair(TJSONPair.Create('size',
        TJSONNumber.Create(AQuery.Fields.ByNameAsInteger['Length'])));
    Result := Obj;
  end;

var
  Query: TUIBQuery;
begin
  Result := TJSONArray.Create;
  Query := FB.Prepare(Format(GET_TABLE_INFO, [TableName]));
  try
    Query.Open;
    while not Query.Eof do
    begin
      if Trim(Query.Fields.ByNameAsString['Constraint']) <> 'PRIMARY KEY' then
        Result.AddElement(GetFieldMapping(Query));
      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

function TFirebirdMappingCreatorStrategy.CreatePKFieldMapping(
  TableName: String): TJSONObject;
var
  Query: TUIBQuery;
  RawFieldType: String;
  dormFieldType: string;
  RawFieldName: string;
  ThisClassAlreadyHaveAnOID: Boolean;
begin
  ThisClassAlreadyHaveAnOID := false;
  Query := FB.Prepare(Format(GET_TABLE_INFO, [TableName]));
  try
    Query.Open;
    while not Query.Eof do
    begin
      { todo: Check the case with compound key }
      // What happend if there is a compound key?
      if Trim(Query.Fields.ByNameAsString['Constraint']) = 'PRIMARY KEY' then
      begin
        if ThisClassAlreadyHaveAnOID then
          raise Exception.Create('Not supported! Table ' + TableName +
            ' have a compound key');
        ThisClassAlreadyHaveAnOID := true;
        Result := TJSONObject.Create;
        RawFieldType := Trim(Query.Fields.ByNameAsString['Type']);
        RawFieldName := Trim(Query.Fields.ByNameAsString['Field']);
        if (RawFieldType = 'INT64') or (RawFieldType = 'INTEGER') or
          (RawFieldType = 'SMALLINT') then
          dormFieldType := 'integer'
        else if (RawFieldType = 'VARCHAR') then
          dormFieldType := 'string'
        else
        begin
          raise Exception.Create('Cannot map field ' + TableName + '.' +
            RawFieldName + ' of type ' + RawFieldType + ' as primary key');
        end;

        Result
          .AddPair('name', FieldNameToClassAtribute(RawFieldName))
          .AddPair('field', RawFieldName)
          .AddPair('field_type', dormFieldType);
      end;
      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

destructor TFirebirdMappingCreatorStrategy.Destroy;
begin
  if Assigned(FConfiguration) then
    FConfiguration.Free;
  inherited;
end;

procedure TFirebirdMappingCreatorStrategy.Disconnect;
begin
  inherited;
  FB.Free;
end;

function TFirebirdMappingCreatorStrategy.GetTablesName: TList<string>;
var
  Query: TUIBQuery;
begin
  Query := FB.Prepare(GET_TABLES_AND_VIEWS);
  try
    Result := TList<string>.Create;
    Query.Open;
    while not Query.Eof do
    begin
      Result.Add(Trim(Query.Fields.ByNameAsString['NAME']));
      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TFirebirdMappingCreatorStrategy.InitUserParameters;
begin
  AddParameter(CONNECTION_STRING, 'host:alias or host:full_file_path');
  AddParameter(USERNAME, 'sysdba');
  AddParameter(PASSWORD, 'masterkey');
  AddParameter(PACKAGE, 'BusinessObjects');
end;

function TFirebirdMappingCreatorStrategy.InternalExecute(AOutputStream
  : TStream): Boolean;
var
  Tables: TList<string>;
  TableMapping, json: TJSONObject;
  TableName: String;
  sw: TStreamWriter;
  sojson: ISuperObject;
begin
  Result := true;
  try
    json := TJSONObject.Create;
    try
      Tables := GetTablesName;
      try
        for TableName in Tables do
        begin
          TableMapping := TJSONObject.Create;
          json.AddPair(TableNameToClassName(TableName), TableMapping);
          TableMapping
            .AddPair('package', GetUserParameterByName(PACKAGE))
            .AddPair('table', TableName)
            .AddPair('id', CreatePKFieldMapping(TableName))
            .AddPair('fields', CreateFieldsMapping(TableName))
            .AddPair('has_many', TJSONArray.Create)
            .AddPair('has_one', TJSONArray.Create)
            .AddPair('belongs_to', TJSONArray.Create);
        end;
      finally
        Tables.Free;
      end;
      sw := TStreamWriter.Create(AOutputStream);
      try
        sojson := SO(json.ToString);
        sw.Write(sojson.AsJSon(true));
        sojson := nil;
      finally
        sw.Free;
      end;
    finally
      json.Free;
    end;
  except
    on E: Exception do
    begin
      Errors.Add(E.ClassName + ': ' + E.Message);
      Result := false;
    end;
  end;
end;

{ TInterbaseMappingCreatorStrategy }

procedure TInterbaseMappingCreatorStrategy.Connect;
begin
  raise Exception.Create('Still not implemented :-(');
end;

function TInterbaseMappingCreatorStrategy.InternalExecute(
  AOutputStream: TStream): Boolean;
begin

end;

initialization

TMappingCreatorStrategyRegister.Register
  (TFirebirdMappingCreatorStrategy,
  'Firebird');
TMappingCreatorStrategyRegister.Register
  (TInterbaseMappingCreatorStrategy,
  'Interbase');

end.
