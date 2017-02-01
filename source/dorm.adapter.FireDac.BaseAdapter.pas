unit dorm.adapter.FireDAC.BaseAdapter;


////////////////////////
//Contributors
//- Marco Mottadelli
//- Mauro Catellani
////////////////////////

interface

uses
  dorm.Commons,
  dorm.Mappings,
  classes,
  SysUtils,
  DB,
  Rtti,
  dorm,
  superobject,
  TypInfo,
  FMTBcd,
  dorm.Filters,
  Generics.Collections,
  dorm.Collections,
  dorm.adapter.FireDAC.Facade,
  dorm.adapter.Base,
  dorm.Mappings.Strategies,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param;

type
  TFireDACBaseAdapter = class(TBaseAdapter, IdormPersistStrategy)
  private
    function GetFireDACReaderFor(ARttiType: TRttiType; AMappingTable: TMappingTable; const Value: TValue;
      AMappingRelationField: TMappingField = nil): TFDQuery;
    function GetSqlFieldNames(AMappingTable: TMappingTable; AObject: TObject): string;
    function GetSqlFieldValues(AMappingTable: TMappingTable; AObject: TObject): string;
    function GetSqlFieldsForUpdate(AMappingTable: TMappingTable; AObject: TObject): string;
    procedure SetNullParameterValue(AStatement: TFDQuery; ParameterIndex: Integer);
  protected
    FFormatSettings: TFormatSettings;
    FD: TFireDACFacade;
    FLogger: IdormLogger;
    procedure InitFormatSettings;
    function CreateFireDACFacade(Conf: ISuperObject): TFireDACFacade; virtual; abstract;
    function CreateObjectFromFireDACQuery(ARttiType: TRttiType; AReader: TFDQuery;
      AMappingTable: TMappingTable): TObject;
    procedure LoadObjectFromFireDACReader(AObject: TObject; ARttiType: TRttiType; AReader: TFDQuery;
      AFieldsMapping: TMappingFieldList);
    function GetLogger: IdormLogger;
    procedure SetFireDACParameterValue(AFieldType: string; AStatement: TFDQuery; ParameterIndex: Integer;
      AValue: TValue; AIsNullable: boolean = False);
    function EscapeDateTime(const Value: TDate): string; override;
  public
    // Start Method Interface IdormPersistStrategy
    function GetLastInsertOID: TValue;
    function Insert(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable): TValue;
    function Update(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable;
      ACurrentVersion: Int64): Int64;
    function Delete(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable;
      ACurrentVersion: Int64): Int64;
    procedure DeleteAll(AMappingTable: TMappingTable);
    function Count(AMappingTable: TMappingTable): Int64;
    function Load(ARttiType: TRttiType; AMappingTable: TMappingTable; AMappingRelationField: TMappingField;
      const Value: TValue; AObject: TObject): boolean; overload;
    function Load(ARttiType: TRttiType; AMappingTable: TMappingTable; const Value: TValue; AObject: TObject)
      : boolean; overload;
    procedure LoadList(AList: TObject; ARttiType: TRttiType; AMappingTable: TMappingTable;
      ACriteria: ICriteria); overload;
    procedure ConfigureStrategy(ConfigurationInfo: ISuperObject); virtual;
    procedure InitStrategy;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function InTransaction: boolean;
    procedure SetLogger(ALogger: IdormLogger);
    function RawExecute(SQL: string): Int64;
    function ExecuteAndGetFirst(SQL: string): Int64;
    function GetDatabaseBuilder(AEntities: TList<string>; AMappings: ICacheMappingStrategy): IDataBaseBuilder;
    function ExecuteCommand(ACommand: IdormCommand): Int64;
    // End Method Interface IdormPersistStrategy

    destructor Destroy; override;
    class procedure register;
  end;

implementation

uses
  dorm.Utils;

procedure TFireDACBaseAdapter.InitFormatSettings;
begin
  FFormatSettings.LongDateFormat := 'YYYY-MM-DD';
  FFormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  FFormatSettings.LongTimeFormat := 'HH:NN:SS';
  FFormatSettings.ShortTimeFormat := 'HH:NN:SS';
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.TimeSeparator := ':';
end;

procedure TFireDACBaseAdapter.InitStrategy;
begin
end;

function TFireDACBaseAdapter.Update(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable;
  ACurrentVersion: Int64): Int64;
var
  field: TMappingField;
  SQL: string;
  Query: TFDQuery;
  I, pk_idx: Integer;
  v: TValue;
  sql_fields_names: string;
  pk_field: string;
  isTransient: boolean;
  isNullable: boolean;
begin
  sql_fields_names := GetSqlFieldsForUpdate(AMappingTable, AObject);
  pk_field := AMappingTable.Fields[GetPKMappingIndex(AMappingTable.Fields)].FieldName;
  SQL := Format('UPDATE %S SET %S WHERE [%S] = :' + pk_field, [AMappingTable.TableName, sql_fields_names, pk_field]);
  if ACurrentVersion >= 0 then
  begin
    SQL := SQL + ' AND OBJVERSION = ' + IntToStr(ACurrentVersion);
  end;
  GetLogger.Debug(AMappingTable.Fields[GetPKMappingIndex(AMappingTable.Fields)].FieldName);
  GetLogger.Debug('NEW QUERY: ' + SQL);
  Query := FD.NewQuery;
  Query.SQL.Text := SQL;
  try
    I := 0;
    for field in AMappingTable.Fields do
    begin
      // manage transients fields
      isTransient := TdormUtils.HasAttribute<Transient>(field.RTTICache.RTTIProp);
      // manage nullable fields
      isNullable := TdormUtils.HasAttribute<Nullable>(field.RTTICache.RTTIProp);

      if (not field.IsPK) and (not isTransient) then
      begin
        v := TdormUtils.GetField(AObject, field.name);
        SetFireDACParameterValue(field.FieldType, Query, I, v, isNullable);
        inc(I);
      end;
    end;
    // Retrieve pk index
    pk_idx := GetPKMappingIndex(AMappingTable.Fields);
    // Retrieve pk value
    v := ARttiType.GetProperty(AMappingTable.Fields[pk_idx].name).GetValue(AObject);
    // Set pk parameter value
    SetFireDACParameterValue(AMappingTable.Fields[pk_idx].FieldType, Query, I, v);

    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    Result := FD.Execute(Query);
  finally
    Query.Free;
  end;
end;

procedure TFireDACBaseAdapter.Commit;
begin
  FD.CommitTransaction;
end;

procedure TFireDACBaseAdapter.ConfigureStrategy(ConfigurationInfo: ISuperObject);
begin
  FD := CreateFireDACFacade(ConfigurationInfo);
  // TODO: We must implement here keys generator part for other database
  // In SqlServer this part isn't necessary because sequence is missing

  inherited;
end;

function TFireDACBaseAdapter.Count(AMappingTable: TMappingTable): Int64;
var
  Qry: TFDQuery;
  SQL: string;
begin
  Result := -1;
  SQL := 'SELECT COUNT(*) AS CNT FROM ' + AMappingTable.TableName;
  GetLogger.Debug('PREPARING: ' + SQL);
  Qry := FD.NewQuery;
  try
    Qry.SQL.Text := SQL;
    Qry.Open;
    if not Qry.Eof then
      Result := Qry.FieldByName('CNT').AsLargeInt;
    Qry.Close;
  finally
    Qry.Free;
  end;
end;

function TFireDACBaseAdapter.Delete(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable;
  ACurrentVersion: Int64): Int64;
var
  pk_idx: Integer;
  pk_value: TValue;
  pk_attribute_name, pk_field_name, SQL: string;
  Qry: TFDQuery;
begin
  pk_idx := GetPKMappingIndex(AMappingTable.Fields);
  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + AMappingTable.TableName);
  pk_attribute_name := AMappingTable.Fields[pk_idx].name;
  pk_field_name := AMappingTable.Fields[pk_idx].FieldName;
  pk_value := ARttiType.GetProperty(pk_attribute_name).GetValue(AObject);
  SQL := 'DELETE FROM ' + AMappingTable.TableName + ' WHERE [' + pk_field_name + '] = :' + pk_field_name;
  if ACurrentVersion >= 0 then
    SQL := SQL + ' AND OBJVERSION = ' + IntToStr(ACurrentVersion);

  GetLogger.Debug('NEW QUERY: ' + SQL);
  Qry := FD.NewQuery;
  try
    Qry.SQL.Text := SQL;
    Qry.Params[0].DataType := ftLargeint;
    Qry.Params[0].AsLargeInt := pk_value.AsInt64;
    GetLogger.Debug('EXECUTING QUERY: ' + SQL);
    Qry.ExecSQL;
    Result := Qry.RowsAffected;
  finally
    Qry.Free;
  end;
end;

procedure TFireDACBaseAdapter.DeleteAll(AMappingTable: TMappingTable);
var
  SQL: string;
begin
  SQL := 'DELETE FROM ' + AMappingTable.TableName;
  GetLogger.Debug('EXECUTING :' + SQL);
  FD.Execute(SQL);
end;

destructor TFireDACBaseAdapter.Destroy;
begin
  FD.Free;
  inherited;
end;

function TFireDACBaseAdapter.EscapeDateTime(const Value: TDate): string;
begin
  Result := FormatDateTime('YYYY-MM-DD&HH:NN:SS', Value);
  Result := StringReplace(Result, '&', 'T', [rfReplaceAll]);
end;

function TFireDACBaseAdapter.ExecuteAndGetFirst(SQL: string): Int64;
var
  Qry: TFDQuery;
begin
  GetLogger.EnterLevel('ExecuteAndGetFirst');
  Result := 0;
  GetLogger.Info('PREPARING: ' + SQL);
  Qry := FD.NewQuery;
  try
    GetLogger.Info('EXECUTING: ' + SQL);
    Qry.SQL.Text := SQL;
    Qry.Open;
    if not Qry.Eof then
      Result := Int64(Qry.Fields[0].AsLargeInt)
    else
      raise EdormException.Create('ExecuteAndGetFirst returns 0 rows');
    Qry.Close;
  finally
    GetLogger.ExitLevel('ExecuteAndGetFirst');
    Qry.Free;
  end;
end;

function TFireDACBaseAdapter.ExecuteCommand(ACommand: IdormCommand): Int64;
var
  SQL: string;
  Qry: TFDQuery;
begin
  SQL := ACommand.GetSQL;
  GetLogger.Debug('EXECUTING: ' + SQL);
  Qry := FD.NewQuery;
  try
    // if reader.Params.ParamCount <> 0 then
    // raise EdormException.Create('Parameters not replaced');
    Qry.SQL.Text := SQL;
    Qry.Execute;
    Result := Qry.RowsAffected;
  finally
    Qry.Free;
  end;
end;

function TFireDACBaseAdapter.GetDatabaseBuilder(AEntities: TList<string>; AMappings: ICacheMappingStrategy)
  : IDataBaseBuilder;
begin
  AEntities.Free; // just to hide the memory leak
  raise Exception.Create('Not implemented for ' + self.ClassName);
end;

function TFireDACBaseAdapter.GetLogger: IdormLogger;
begin
  Result := FLogger;
end;

function TFireDACBaseAdapter.Insert(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable): TValue;
var
  field: TMappingField;
  sql_fields_names, sql_fields_values, SQL: String;
  Query: TFDQuery;
  I, pk_idx: Integer;
  v, pk_value: TValue;
  isTransient: boolean;
  isNullable: boolean;
begin
  sql_fields_names := '';
  sql_fields_values := '';

  for field in AMappingTable.Fields do
  begin
    // manage transients fields
    isTransient := TdormUtils.HasAttribute<Transient>(field.RTTICache.RTTIProp);
    // manage nullable fields
    isNullable := TdormUtils.HasAttribute<Nullable>(field.RTTICache.RTTIProp);

    if (not field.IsPK) and (not isTransient) then
    begin
      v := TdormUtils.GetField(AObject, field.RTTICache);
      // Compose Fields Names and Values
      sql_fields_names := sql_fields_names + ',[' + field.FieldName + ']';
      sql_fields_values := sql_fields_values + ',:' + field.FieldName;
    end;
  end;
  System.Delete(sql_fields_names, 1, 1);
  System.Delete(sql_fields_values, 1, 1);

  SQL := Format('INSERT INTO %s (%s) VALUES (%s)', [AMappingTable.TableName, sql_fields_names, sql_fields_values]);
  GetLogger.Debug('PREPARING :' + SQL);

  Query := FD.NewQuery;
  Query.SQL.Text := SQL;
  try
    I := 0;
    for field in AMappingTable.Fields do
    begin
      v := TdormUtils.GetField(AObject, field.RTTICache);
      // manage transients fields
      isTransient := TdormUtils.HasAttribute<Transient>(field.RTTICache.RTTIProp);
      // manage nullable fields
      isNullable := TdormUtils.HasAttribute<Nullable>(field.RTTICache.RTTIProp);

      if (not field.IsPK) and (not isTransient) then
      begin
        v := TdormUtils.GetField(AObject, field.RTTICache);
        SetFireDACParameterValue(field.FieldType, Query, I, v, isNullable);
        inc(I);
      end;
    end;
    GetLogger.Debug('EXECUTING PREPARED :' + string(SQL));
    FD.Execute(Query);
  finally
    Query.Free;
  end;
  pk_idx := GetPKMappingIndex(AMappingTable.Fields);
  pk_value := GetLastInsertOID;
  TdormUtils.SetField(AObject, AMappingTable.Fields[pk_idx].RTTICache, pk_value);
  Result := pk_value;
end;

function TFireDACBaseAdapter.InTransaction: boolean;
var
  tr: TFDTransaction;
begin
  tr := FD.GetCurrentTransaction;
  Result := assigned(tr);
  if Result then
    Result := tr.Active;
end;

function TFireDACBaseAdapter.GetLastInsertOID: TValue;
var
  Qry: TFDQuery;
  SQL: String;
begin
  SQL := 'SELECT @@IDENTITY AS LAST_IDENTITY';
  GetLogger.Debug('PREPARING: ' + SQL);
  Qry := FD.NewQuery;
  try
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    Qry.SQL.Text := SQL;
    Qry.Open;
    if not Qry.Eof then
    begin
      if (not Qry.FieldByName('LAST_IDENTITY').IsNull) then
      begin
        Result := Qry.FieldByName('LAST_IDENTITY').AsLargeInt;
      end
      else
        Result := TValue.Empty;
    end;
    Qry.Close;
  finally
    Qry.Free;
  end;
end;

function TFireDACBaseAdapter.GetSqlFieldNames(AMappingTable: TMappingTable; AObject: TObject): string;
var
  v: TValue;
  field: TMappingField;
  isTransient: boolean;
  isNullable: boolean;
begin
  Result := '';
  for field in AMappingTable.Fields do
  begin
    // manage transients fields
    isTransient := TdormUtils.HasAttribute<Transient>(field.RTTICache.RTTIProp);
    // manage nullable fields
    isNullable := TdormUtils.HasAttribute<Nullable>(field.RTTICache.RTTIProp);

    if (not field.IsPK) and (not isTransient) then
    begin
      v := TdormUtils.GetField(AObject, field.name);
      Result := Result + ',[' + field.FieldName + ']';
    end;
  end;
  System.Delete(Result, 1, 1);
end;

function TFireDACBaseAdapter.GetSqlFieldValues(AMappingTable: TMappingTable; AObject: TObject): string;
var
  field: TMappingField;
  isTransient: boolean;
  isNullable: boolean;
  v: TValue;
begin
  Result := '';
  for field in AMappingTable.Fields do
  begin
    // manage transients fields
    isTransient := TdormUtils.HasAttribute<Transient>(field.RTTICache.RTTIProp);
    // manage nullable fields
    isNullable := TdormUtils.HasAttribute<Nullable>(field.RTTICache.RTTIProp);

    if (not field.IsPK) and (not isTransient) then
    begin
      v := TdormUtils.GetField(AObject, field.name);
      Result := Result + ',:' + field.FieldName;
    end;
  end;
  System.Delete(Result, 1, 1);
end;

function TFireDACBaseAdapter.GetSqlFieldsForUpdate(AMappingTable: TMappingTable; AObject: TObject): string;
var
  field: TMappingField;
  isTransient: boolean;
  isNullable: boolean;
  v: TValue;
begin
  Result := '';
  for field in AMappingTable.Fields do
  begin
    // manage transients fields
    isTransient := TdormUtils.HasAttribute<Transient>(field.RTTICache.RTTIProp);
    // manage nullable fields
    isNullable := TdormUtils.HasAttribute<Nullable>(field.RTTICache.RTTIProp);

    if (not field.IsPK) and (not isTransient) then
    begin
      v := TdormUtils.GetField(AObject, field.name);
      Result := Result + ',[' + field.FieldName + '] = :' + field.FieldName;
    end;
  end;
  System.Delete(Result, 1, 1);
end;

function TFireDACBaseAdapter.GetFireDACReaderFor(ARttiType: TRttiType; AMappingTable: TMappingTable;
  const Value: TValue; AMappingRelationField: TMappingField): TFDQuery;
var
  pk_idx: Integer;
  pk_field_name, SQL: string;
begin
  if AMappingRelationField = nil then
  begin
    pk_idx := GetPKMappingIndex(AMappingTable.Fields);
    if pk_idx = -1 then
      raise Exception.Create('Invalid primary key for table ' + AMappingTable.TableName);
    pk_field_name := AMappingTable.Fields[pk_idx].FieldName;
    SQL := 'SELECT ' + GetSelectFieldsList(AMappingTable.Fields, true) + ' FROM ' + AMappingTable.TableName + ' WHERE ['
      + pk_field_name + '] = :' + pk_field_name;
  end
  else
  begin
    pk_idx := GetPKMappingIndex(AMappingTable.Fields);
    if pk_idx = -1 then
      raise Exception.Create('Invalid primary key for table ' + AMappingTable.TableName);
    pk_field_name := AMappingTable.Fields[pk_idx].FieldName;
    SQL := 'SELECT ' + GetSelectFieldsList(AMappingTable.Fields, true) + ' FROM ' + AMappingTable.TableName + ' WHERE ['
      + AMappingRelationField.FieldName + '] = :' + pk_field_name;
  end;
  GetLogger.Debug('PREPARING: ' + SQL);
  Result := FD.NewQuery;
  Result.SQL.Text := SQL;
  if Value.IsOrdinal then begin
    Result.Params[0].DataType := ftLargeint;
    Result.Params[0].AsLargeInt := Value.AsInt64;
  end else begin
    Result.Params[0].DataType := ftString;
    Result.Params[0].AsString := Value.AsString;
  end;
end;

function TFireDACBaseAdapter.Load(ARttiType: TRttiType; AMappingTable: TMappingTable;
  AMappingRelationField: TMappingField; const Value: TValue; AObject: TObject): boolean;
var
  reader: TFDQuery;
begin
  reader := GetFireDACReaderFor(ARttiType, AMappingTable, Value, AMappingRelationField);
  try
    reader.Open();
    Result := not reader.Eof;
    if Result then
      LoadObjectFromFireDACReader(AObject, ARttiType, reader, AMappingTable.Fields);
    reader.Next;
    if not reader.Eof then
      // there is some problem.... here I should have only one record
      raise EdormException.Create('Singleton select returns more than 1 record');
  finally
    reader.Free;
  end;
end;

procedure TFireDACBaseAdapter.LoadList(AList: TObject; ARttiType: TRttiType; AMappingTable: TMappingTable;
  ACriteria: ICriteria);
var
  SQL: string;
  reader: TFDQuery;
  CustomCriteria: ICustomCriteria;
begin
  if assigned(ACriteria) and TInterfacedObject(ACriteria).GetInterface(ICustomCriteria, CustomCriteria) then
    SQL := CustomCriteria.GetSQL
  else
    SQL := self.GetSelectSQL(ACriteria, AMappingTable);
  GetLogger.Debug('EXECUTING: ' + SQL);
  reader := FD.NewQuery;
  // if reader.Params.ParamCount <> 0 then
  // raise EdormException.Create('Parameters not replaced');
  reader.SQL.Text := SQL;
  reader.Open();
  try
    while not reader.Eof do
    begin
      TdormUtils.MethodCall(AList, 'Add', [CreateObjectFromFireDACQuery(ARttiType, reader, AMappingTable)]);
      reader.Next;
    end;
    reader.Close;
  finally
    reader.Free;
  end;
end;

procedure TFireDACBaseAdapter.LoadObjectFromFireDACReader(AObject: TObject; ARttiType: TRttiType; AReader: TFDQuery;
  AFieldsMapping: TMappingFieldList);
var
  field: TMappingField;
  v: TValue;
  S: string;
  sourceStream: TStringStream;
  f: TField;
begin
  try
    for field in AFieldsMapping do
    begin
      if CompareText(field.FieldType, 'string') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsString;
        S := field.FieldName + ' as string';
      end
      else if CompareText(field.FieldType, 'integer') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsInteger;
        S := field.FieldName + ' as integer';
      end
      else if CompareText(field.FieldType, 'date') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        S := field.FieldName + ' as date';
      end
      else if CompareText(field.FieldType, 'blob') = 0 then
      begin
        S := field.FieldName + ' as blob';
        sourceStream := nil;
        if not AReader.FieldByName(field.FieldName).IsNull then
        begin
          sourceStream := TStringStream.Create(AReader.FieldByName(field.FieldName).AsBytes);
        end;
        if assigned(sourceStream) then
        begin
          sourceStream.Position := 0;
          v := sourceStream;
        end
        else
          v := nil;
      end
      else if CompareText(field.FieldType, 'decimal') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsFloat;
        S := field.FieldName + ' as decimal';
      end
      else if CompareText(field.FieldType, 'boolean') = 0 then
      begin
        f := AReader.FieldByName(field.FieldName);
        if f.DataType = ftBoolean then
        begin
          v := AReader.FieldByName(field.FieldName).AsBoolean
        end
        else
        begin
          v := AReader.FieldByName(field.FieldName).AsInteger <> 0;
        end;
        S := field.FieldName + ' as boolean';
      end
      else if CompareText(field.FieldType, 'datetime') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        S := field.FieldName + ' as datetime';
      end
      else if CompareText(field.FieldType, 'time') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        S := field.FieldName + ' as time';
      end
      else if CompareText(field.FieldType, 'float') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsFloat;
        S := field.FieldName + ' as float';
      end
      else
        raise Exception.Create('Unknown field type for ' + field.FieldName);
      try
        TdormUtils.SetField(AObject, field.name, v);
      except
        on E: Exception do
        begin
          raise EdormException.Create(E.Message + sLineBreak + '. Probably cannot write ' + ARttiType.ToString
            + '.' + S);
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
end;

function TFireDACBaseAdapter.RawExecute(SQL: string): Int64;
begin
  GetLogger.Warning('RAW EXECUTE: ' + SQL);
  Result := FD.Execute(SQL);
end;

function TFireDACBaseAdapter.CreateObjectFromFireDACQuery(ARttiType: TRttiType; AReader: TFDQuery;
  AMappingTable: TMappingTable): TObject;
var
  obj: TObject;
  field: TMappingField;
  v: TValue;
  S: string;
  targetStream: TMemoryStream;
  f: TField;
begin
  try
    obj := TdormUtils.CreateObject(ARttiType);
    for field in AMappingTable.Fields do
    begin
      if CompareText(field.FieldType, 'string') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsString;
        S := field.FieldName + ' as string';
      end
      else if CompareText(field.FieldType, 'integer') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsInteger;
        S := field.FieldName + ' as integer';
      end
      else if CompareText(field.FieldType, 'date') = 0 then
      begin
        v := trunc(AReader.FieldByName(field.FieldName).AsDateTime);
        S := field.FieldName + ' as date';
      end
      else if CompareText(field.FieldType, 'blob') = 0 then
      begin
        targetStream := nil;
        if not AReader.FieldByName(field.FieldName).IsNull then
        begin
          targetStream := TStringStream.Create(AReader.FieldByName(field.FieldName).AsBytes);
          // targetStream := TMemoryStream(AReader.CreateBlobStream(AReader.FieldByName(field.FieldName), bmRead));
          targetStream.Position := 0;
        end;
        S := field.FieldName + ' as blob';
        v := targetStream;
      end
      else if CompareText(field.FieldType, 'decimal') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsFloat;
        S := field.FieldName + ' as decimal';
      end
      else if CompareText(field.FieldType, 'float') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsFloat;
        S := field.FieldName + ' as float';
      end
      else if CompareText(field.FieldType, 'boolean') = 0 then
      begin
        f := AReader.FieldByName(field.FieldName);
        if f.DataType = ftBoolean then
        begin
          v := AReader.FieldByName(field.FieldName).AsBoolean
        end
        else
        begin
          v := AReader.FieldByName(field.FieldName).AsInteger <> 0;
        end;
        S := field.FieldName + ' as boolean';
      end
      else if CompareText(field.FieldType, 'datetime') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        S := field.FieldName + ' as datetime';
      end
      else if CompareText(field.FieldType, 'time') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        S := field.FieldName + ' as datetime';
      end
      else
        raise Exception.Create('Unknown field type for ' + field.FieldName);
      try
        TdormUtils.SetField(obj, field.name, v);
      except
        on E: Exception do
        begin
          raise EdormException.Create(E.Message + sLineBreak + '. Probably cannot write ' + ARttiType.ToString
            + '.' + S);
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      raise;
    end;
  end;
  Result := obj;
end;

class procedure TFireDACBaseAdapter.register;
begin
  //
end;

procedure TFireDACBaseAdapter.Rollback;
begin
  FD.GetCurrentTransaction.Rollback;
end;

procedure TFireDACBaseAdapter.SetFireDACParameterValue(AFieldType: string; AStatement: TFDQuery;
  ParameterIndex: Integer; AValue: TValue; AIsNullable: boolean);
var
  sourceStream: TStream;
  str: TStringStream;
begin
  if CompareText(AFieldType, 'string') = 0 then
  begin
    AStatement.Params[ParameterIndex].DataType := ftString;
    AStatement.Params[ParameterIndex].AsString := AValue.AsString;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + AValue.AsString);
  end
  else if CompareText(AFieldType, 'decimal') = 0 then
  begin
    if (AValue.AsExtended = 0.0) and AIsNullable then
      SetNullParameterValue(AStatement, ParameterIndex)
    else
    begin
      AStatement.Params[ParameterIndex].DataType := ftFloat;
      AStatement.Params[ParameterIndex].AsFloat := AValue.AsExtended;
      GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + FloatToStr(AValue.AsExtended));
    end;
  end
  else if CompareText(AFieldType, 'float') = 0 then
  begin
    if (AValue.AsExtended = 0.0) and AIsNullable then
      SetNullParameterValue(AStatement, ParameterIndex)
    else
    begin
      AStatement.Params[ParameterIndex].DataType := ftFloat;
      AStatement.Params[ParameterIndex].AsFloat := AValue.AsExtended;
      GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + FloatToStr(AValue.AsExtended));
    end;
  end
  else if CompareText(AFieldType, 'integer') = 0 then
  begin
    if (AValue.AsInt64 = 0) and AIsNullable then
      SetNullParameterValue(AStatement, ParameterIndex)
    else
    begin
      AStatement.Params[ParameterIndex].DataType := ftLargeint;
      AStatement.Params[ParameterIndex].AsLargeInt := AValue.AsInt64;
      GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + IntToStr(AValue.AsInt64));
    end;
  end
  else if CompareText(AFieldType, 'boolean') = 0 then
  begin
    AStatement.Params[ParameterIndex].DataType := ftBoolean;
    AStatement.Params[ParameterIndex].AsBoolean := AValue.AsBoolean;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + BoolToStr(AValue.AsBoolean, true));
  end
  else if CompareText(AFieldType, 'date') = 0 then
  begin
    if (AValue.AsExtended = 0) and AIsNullable then
      SetNullParameterValue(AStatement, ParameterIndex)
    else
    begin
      AStatement.Params[ParameterIndex].DataType := ftDate;
      AStatement.Params[ParameterIndex].AsDate := trunc(AValue.AsExtended);
      GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + EscapeDate(trunc(AValue.AsExtended)));
    end;
  end
  else if CompareText(AFieldType, 'datetime') = 0 then
  begin
    if (AValue.AsExtended = 0) and AIsNullable then
      SetNullParameterValue(AStatement, ParameterIndex)
    else
    begin
      AStatement.Params[ParameterIndex].DataType := ftDateTime;
      AStatement.Params[ParameterIndex].AsDateTime := AValue.AsExtended;
      GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + EscapeDate(AValue.AsExtended));
    end;
  end
  else if CompareText(AFieldType, 'time') = 0 then
  begin
    if (AValue.AsExtended = 0) and AIsNullable then
      SetNullParameterValue(AStatement, ParameterIndex)
    else
    begin
      AStatement.Params[ParameterIndex].DataType := ftTime;
      AStatement.Params[ParameterIndex].AsDateTime := AValue.AsExtended;
      GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + EscapeDateTime(AValue.AsExtended));
    end;
  end
  else if CompareText(AFieldType, 'blob') = 0 then
  begin
    sourceStream := TStream(AValue.AsObject);
    if sourceStream = nil then
    begin
      AStatement.Params[ParameterIndex].AsBlob := '';
      GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = nil');
    end
    else
    begin
      str := TStringStream.Create;
      try
        sourceStream.Position := 0;
        str.CopyFrom(sourceStream, 0);
        str.Position := 0;
        AStatement.Params[ParameterIndex].AsBlob := str.DataString;
        GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = <blob ' + IntToStr(str.Size) + ' bytes>');
      finally
        str.Free;
      end;
    end;
  end
  else
    raise EdormException.CreateFmt('Parameter type not supported: [%s]', [AFieldType]);
end;

procedure TFireDACBaseAdapter.SetNullParameterValue(AStatement: TFDQuery; ParameterIndex: Integer);
begin
  AStatement.Params[ParameterIndex].DataType := ftString;
  AStatement.Params[ParameterIndex].AsString := '';
  GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ');
end;

procedure TFireDACBaseAdapter.SetLogger(ALogger: IdormLogger);
begin
  FLogger := ALogger;
end;

procedure TFireDACBaseAdapter.StartTransaction;
begin
  FD.GetConnection; // ensure database connected
  FD.StartTransaction;
end;

function TFireDACBaseAdapter.Load(ARttiType: TRttiType; AMappingTable: TMappingTable; const Value: TValue;
  AObject: TObject): boolean;
var
  reader: TFDQuery;
begin
  reader := GetFireDACReaderFor(ARttiType, AMappingTable, Value);
  try
    reader.Open();
    Result := not reader.Eof;
    if Result then
      LoadObjectFromFireDACReader(AObject, ARttiType, reader, AMappingTable.Fields);
  finally
    reader.Free;
  end;
end;

end.
