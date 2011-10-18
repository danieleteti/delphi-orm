unit dorm.adapter.Firebird;

interface

uses
  dorm.Commons,
  classes,
  SysUtils,
  DB,
  SqlExpr,
  dorm.adapter.Firebird.Factory,
  DBXClient,
  DBXCommon,
  dbclient,
  dbxfirebird,
  Rtti,
  dorm,
  superobject,
  TypInfo,
  FMTBcd,
  dorm.Collections;

type
  TFirebirdPersistStrategy = class(TdormInterfacedObject, IdormPersistStrategy)
  strict private
    FLogger: IdormLogger;
    FCurTransaction: TDBXTransaction;
    FKeysGeneratorClassName: string;
    FKeysGenerator: IdormKeysGenerator;
    FKeyType: TdormKeyType;
    FNullKeyValue: TValue;
    FB: TFBFactory;
  strict protected
    function CreateObjectFromDBXReader(ARttiType: TRttiType;
      AReader: TDBXReader; AFieldsMapping: TArray<TdormFieldMapping>): TObject;
    function LoadObjectFromDBXReader(ARttiType: TRttiType; AReader: TDBXReader;
      AFieldsMapping: TArray<TdormFieldMapping>; AObject: TObject): Boolean;
    function GetSelectList(AMapping: TArray<TdormFieldMapping>): string;
    function GetLogger: IdormLogger;
    procedure SetDBXParameterValue(aFieldType: string;
      aParameter: TDBXParameter; aValue: TValue);
    procedure SetDBXValue(aFieldType: string; aDBXValue: TDBXWritableValue;
      aValue: TValue);
  public
    function GenerateAndFillPrimaryKeyParam(PKParam: TDBXParameter;
      const Entity: string): TValue; overload;
    function FillPrimaryKeyParam(PKParam: TDBXParameter;
      const Value: TValue): TValue;
    // function FillPrimaryKeyParam(PKParam: TDBXParameter; const Value: string): TValue; overload;
    function GetLastInsertID: Int64;
    function GetKeysGenerator: IdormKeysGenerator;
    function Insert(rtti_type: TRttiType; AObject: TObject; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>): TValue;
    function Update(rtti_type: TRttiType; AObject: TObject; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>): TValue;
    function Load(ARttiType: TRttiType; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>; const Value: TValue)
      : TObject; overload;
    function Load(ARttiType: TRttiType; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>; const Value: string;
      out AObject: TObject): Boolean; overload;
    function Load(ARttiType: TRttiType; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>; const Value: Integer)
      : TObject; overload;

    function List(ARttiType: TRttiType; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>;
      AdormSearchCriteria: IdormSearchCriteria): TdormCollection; overload;

    function CreateChildLoaderSearch(AChildClassTypeInfo: PTypeInfo;
      AChildClassName, AChildTableName, AChildRelationField: string;
      APKValue: TValue): IdormSearchCriteria;
    function Delete(ARttiType: TRttiType; AObject: TObject; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>): TObject;
    procedure DeleteAll(ATableName: string);
    function Count(ATableName: string): Int64;
    procedure ConfigureStrategy(ConfigurationInfo: ISuperObject);
    procedure InitStrategy;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function InTransaction: Boolean;
    procedure SetLogger(ALogger: IdormLogger);
    destructor Destroy; override;
    class procedure register;
    function IsNullKey(const Value: TValue): Boolean;
    function GetNullKeyValue: TValue;
    function RawExecute(SQL: string): Int64;
  end;

  TFirebirdTableSequence = class(TdormInterfacedObject, IdormKeysGenerator)
  private
    FFirebirdConnection: TFBFactory;
    procedure SetFirebirdConnection(const Value: TFBFactory);
  public
    function NewStringKey(const Entity: string): string;
    function NewIntegerKey(const Entity: string): UInt64;
    property FirebirdConnection: TFBFactory read FFirebirdConnection
      write SetFirebirdConnection;
    class procedure RegisterClass;
  end;

implementation

uses
  dorm.Utils;

function TFirebirdPersistStrategy.Update(rtti_type: TRttiType; AObject: TObject;
  ATableName: string; AFieldsMapping: TArray<TdormFieldMapping>): TValue;
var
  field: TdormFieldMapping;
  // sql_fields_values,
  SQL: string;
  Query: TDBXCommand;
  I, pk_idx: Integer;
  v: TValue;
  sql_fields_names: string;
begin
  sql_fields_names := '';
  for field in AFieldsMapping do
    if not field.pk then
      sql_fields_names := sql_fields_names + ',' + field.field + ' = ?';
  System.Delete(sql_fields_names, 1, 1);

  SQL := Format('UPDATE %S SET %S WHERE %S = ?', [ATableName, sql_fields_names,
    AFieldsMapping[GetPKMappingIndex(AFieldsMapping)].name]);

  GetLogger.Debug('PREPARING: ' + SQL);
  Query := FB.Prepare(SQL);
  try
    I := 0;
    for field in AFieldsMapping do
    begin
      v := TdormUtils.GetField(AObject, field.name);

      if field.pk then
        Continue
      else
      begin
        SetDBXParameterValue(field.field_type, Query.Parameters[I], v);
      end;
      inc(I);
    end;
    pk_idx := GetPKMappingIndex(AFieldsMapping);

    v := rtti_type.GetProperty(AFieldsMapping[pk_idx].name).GetValue(AObject);
    FillPrimaryKeyParam(Query.Parameters[I], v);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    FB.Execute(Query);
  finally
    Query.Free;
  end;
end;

procedure TFirebirdPersistStrategy.Commit;
begin
  if not assigned(FCurTransaction) then
    raise Exception.Create('Transaction is not active');
  FB.GetConnection.CommitFreeAndNil(FCurTransaction);
end;

procedure TFirebirdPersistStrategy.ConfigureStrategy(ConfigurationInfo
  : ISuperObject);
var
  ctx: TRttiContext;
  t: TRttiType;
  obj: TObject;
begin
  FB := TFBFactory.Create(ConfigurationInfo);
  FKeysGeneratorClassName := ConfigurationInfo.S['keys_generator'];
  t := ctx.FindType(FKeysGeneratorClassName);
  if t = nil then
    raise EdormException.Create('Unknown key generator ' +
      FKeysGeneratorClassName);
  obj := t.AsInstance.MetaclassType.Create;
  TFirebirdTableSequence(obj).FFirebirdConnection := FB;
  // VERY VERY VERY BAD!!!

  if not Supports(obj, IdormKeysGenerator, FKeysGenerator) then
    raise EdormException.Create('Invalid keys generator ' +
      FKeysGeneratorClassName);
  if (SameText(ConfigurationInfo.S['key_type'], 'integer')) then
  begin
    FKeyType := ktInteger;
    FNullKeyValue := ConfigurationInfo.I['null_key_value']
  end
  else if (SameText(ConfigurationInfo.S['key_type'], 'string')) then
  begin
    FKeyType := ktString;
    FNullKeyValue := ConfigurationInfo.S['null_key_value']
  end
  else
    raise EdormException.Create('Unknown key type');
  inherited;
end;

function TFirebirdPersistStrategy.Count(ATableName: string): Int64;
var
  cmd: TDBXCommand;
  reader: TDBXReader;
  SQL: string;
begin
  Result := -1;
  SQL := 'SELECT COUNT(*) FROM ' + ATableName;
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);
  try
    reader := cmd.ExecuteQuery;
    try
      if reader.Next then
        Result := reader.Value[0].AsInt64;
    finally
      reader.Free;
    end;
  finally
    cmd.Free;
  end;
end;

function TFirebirdPersistStrategy.CreateChildLoaderSearch(AChildClassTypeInfo
  : PTypeInfo; AChildClassName, AChildTableName, AChildRelationField: string;
  APKValue: TValue): IdormSearchCriteria;
var
  SQL: string;
begin
  SQL := 'SELECT * FROM ' + AChildTableName + ' WHERE ' +
    AChildRelationField + ' = ';
  case FKeyType of
    ktInteger:
      SQL := SQL + IntToStr(APKValue.AsInt64);
    ktString:
      SQL := SQL + '''' + APKValue.AsString + '''';
  end;

  GetLogger.Debug('CreateChildLoaderSearch: ' + SQL);
  Result := TdormSimpleSearchCriteria.Create(nil, SQL);
end;

function TFirebirdPersistStrategy.Delete(ARttiType: TRttiType; AObject: TObject;
  ATableName: string; AFieldsMapping: TArray<TdormFieldMapping>): TObject;
var
  pk_idx: Integer;
  pk_value: TValue;
  pk_attribute_name, pk_field_name, SQL: string;
  cmd: TDBXCommand;
  // reader: TDBXReader;
begin
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + ATableName);
  pk_attribute_name := AFieldsMapping[pk_idx].name;
  pk_field_name := AFieldsMapping[pk_idx].field;
  pk_value := ARttiType.GetProperty(pk_attribute_name).GetValue(AObject);
  SQL := 'DELETE FROM ' + ATableName + ' WHERE ' + pk_field_name + ' = ?';
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);
  try
    FillPrimaryKeyParam(cmd.Parameters[pk_idx], pk_value);
    // cmd.Parameters[0].Value.SetAnsiString(pk_value);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    cmd.ExecuteUpdate;
  finally
    cmd.Free;
  end;

end;

procedure TFirebirdPersistStrategy.DeleteAll(ATableName: string);
var
  SQL: string;
begin
  SQL := 'DELETE FROM ' + ATableName;
  GetLogger.Debug('EXECUTING :' + SQL);
  FB.Execute(SQL);
end;

destructor TFirebirdPersistStrategy.Destroy;
begin
  FB.Free;
  inherited;
end;

function TFirebirdPersistStrategy.GenerateAndFillPrimaryKeyParam
  (PKParam: TDBXParameter; const Entity: string): TValue;
begin
  case FKeyType of
    ktString:
      begin
        PKParam.Value.SetString(FKeysGenerator.NewStringKey(Entity));
        Result := PKParam.Value.GetString;
      end;
    ktInteger:
      begin
        PKParam.Value.SetBcd
          (IntegerToBcd(FKeysGenerator.NewIntegerKey(Entity)));
        Result := BcdToInteger(PKParam.Value.GetBcd);
      end;
  end;

end;

function TFirebirdPersistStrategy.FillPrimaryKeyParam(PKParam: TDBXParameter;
  const Value: TValue): TValue;
begin
  try
    case FKeyType of
      ktString:
        begin
          PKParam.Value.SetString(Value.AsString);
          Result := PKParam.Value.GetString;
        end;
      ktInteger:
        begin
          PKParam.Value.SetBcd(IntegerToBcd(Value.AsInt64));
          Result := BcdToInteger(PKParam.Value.GetBcd);
        end;
    end;
  except
    on E: Exception do
      raise EdormException.Create('Error during fill primary key for query. ' +
        E.Message);
  end;
end;

function TFirebirdPersistStrategy.GetKeysGenerator: IdormKeysGenerator;
begin
  Result := FKeysGenerator;
end;

function TFirebirdPersistStrategy.GetLastInsertID: Int64;
begin
  // Result := FDB.GetLastInsertRowID;
end;

function TFirebirdPersistStrategy.GetLogger: IdormLogger;
begin
  Result := FLogger;
end;

function TFirebirdPersistStrategy.GetSelectList
  (AMapping: TArray<TdormFieldMapping>): string;
var
  field: TdormFieldMapping;
begin
  for field in AMapping do
    Result := Result + ',' + field.field;
  System.Delete(Result, 1, 1);
end;

procedure TFirebirdPersistStrategy.InitStrategy;
begin

end;

function TFirebirdPersistStrategy.Insert(rtti_type: TRttiType; AObject: TObject;
  ATableName: string; AFieldsMapping: TArray<TdormFieldMapping>): TValue;
var
  field: TdormFieldMapping;
  sql_fields_names, sql_fields_values, SQL: ansistring;
  Query: TDBXCommand;
  I, pk_idx: Integer;
  v, pk_value: TValue;
  // par: TDBXParameter;
  // guid: string;
  // d: TDBXDateValue;
begin
  sql_fields_names := '';
  for field in AFieldsMapping do
    // if not field.pk then
    sql_fields_names := sql_fields_names + ',' + field.field;

  System.Delete(sql_fields_names, 1, 1);

  sql_fields_values := '';
  for field in AFieldsMapping do
    // if not field.pk then
    sql_fields_values := sql_fields_values + ',?';
  System.Delete(sql_fields_values, 1, 1);

  SQL := Format('INSERT INTO %s (%S) VALUES (%S)',
    [ATableName, sql_fields_names, sql_fields_values]);
  GetLogger.Debug('PREPARING :' + SQL);
  Query := FB.Prepare(SQL);
  try
    I := 0;
    for field in AFieldsMapping do
    begin
      v := TdormUtils.GetField(AObject, field.name);

      if field.pk then
        pk_value := GenerateAndFillPrimaryKeyParam(Query.Parameters[I],
          ATableName)
      else
      begin
        SetDBXParameterValue(field.field_type, Query.Parameters[I], v);
      end;
      inc(I);
    end;
    GetLogger.Debug('EXECUTING PREPARED :' + SQL);
    FB.Execute(Query);
  finally
    Query.Free;
  end;
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  TdormUtils.SetProperty(AObject, AFieldsMapping[pk_idx].name, pk_value);
  // rtti_type.GetProperty(AFieldsMapping[pk_idx].name)
  // .SetValue(AObject, pk_value);
end;

function TFirebirdPersistStrategy.InTransaction: Boolean;
begin
  Result := assigned(FCurTransaction);
end;

function TFirebirdPersistStrategy.IsNullKey(const Value: TValue): Boolean;
begin
  case FKeyType of
    ktInteger:
      Result := Value.AsInt64 = FNullKeyValue.AsInt64;
    ktString:
      Result := Value.AsString = FNullKeyValue.AsString;
  else
    raise EdormException.Create('Unknown key type');
  end;
end;

function TFirebirdPersistStrategy.GetNullKeyValue: TValue;
begin
  Result := FNullKeyValue;
end;

function TFirebirdPersistStrategy.List(ARttiType: TRttiType; ATableName: string;
  AFieldsMapping: TArray<TdormFieldMapping>;
  AdormSearchCriteria: IdormSearchCriteria): TdormCollection;
var
  // pk_idx: Integer;
  SQL: string;
  cmd: TDBXCommand;
  reader: TDBXReader;
begin
  SQL := AdormSearchCriteria.GetSQL;
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);
  try
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    reader := cmd.ExecuteQuery;
    try
      Result := TdormCollection.Create;
      while reader.Next do
        Result.Add(CreateObjectFromDBXReader(ARttiType, reader,
          AFieldsMapping));
    finally
      reader.Free;
    end;
  finally
    cmd.Free;
  end;
end;

function TFirebirdPersistStrategy.Load(ARttiType: TRttiType; ATableName: string;
  AFieldsMapping: TArray<TdormFieldMapping>; const Value: TValue): TObject;
var
  pk_idx: Integer;
  // , pk_value: Integer;
  pk_attribute_name, pk_field_name, SQL: string;
  cmd: TDBXCommand;
  // v: TValue;
  reader: TDBXReader;
begin
  Result := nil;
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + ATableName);
  pk_attribute_name := AFieldsMapping[pk_idx].name;
  pk_field_name := AFieldsMapping[pk_idx].field;
  SQL := 'SELECT ' + GetSelectList(AFieldsMapping) + ' FROM ' + ATableName +
    ' WHERE ' + pk_field_name + ' = ?';
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);
  try
    FillPrimaryKeyParam(cmd.Parameters[pk_idx], Value);

    // cmd.Parameters[0].Value.SetAnsiString(Value);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    reader := cmd.ExecuteQuery();
    try
      if reader.Next then
        Result := CreateObjectFromDBXReader(ARttiType, reader, AFieldsMapping);
    finally
      reader.Free;
    end;
  finally
    cmd.Free;
  end;
end;

function TFirebirdPersistStrategy.Load(ARttiType: TRttiType; ATableName: string;
  AFieldsMapping: TArray<TdormFieldMapping>; const Value: Integer): TObject;
begin
  // do nothing
end;

function TFirebirdPersistStrategy.Load(ARttiType: TRttiType; ATableName: string;
  AFieldsMapping: TArray<TdormFieldMapping>; const Value: string;
  out AObject: TObject): Boolean;
var
  pk_idx: Integer;
  // , pk_value: Integer;
  pk_attribute_name, pk_field_name, SQL: string;
  cmd: TDBXCommand;
  reader: TDBXReader;
begin
  Result := False;
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + ATableName);
  pk_attribute_name := AFieldsMapping[pk_idx].name;
  pk_field_name := AFieldsMapping[pk_idx].field;
  SQL := 'SELECT ' + GetSelectList(AFieldsMapping) + ' FROM ' + ATableName +
    ' WHERE ' + pk_field_name + ' = ?';
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);
  try
    cmd.Parameters[0].Value.SetAnsiString(Value);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    reader := cmd.ExecuteQuery();
    try
      if reader.Next then
        LoadObjectFromDBXReader(ARttiType, reader, AFieldsMapping, AObject);
    finally
      reader.Free;
    end;
  finally
    cmd.Free;
  end;
end;

function TFirebirdPersistStrategy.LoadObjectFromDBXReader(ARttiType: TRttiType;
  AReader: TDBXReader; AFieldsMapping: TArray<TdormFieldMapping>;
  AObject: TObject): Boolean;
var
  obj: TObject;
  // obj1: TStringBuilder;
  field: TdormFieldMapping;
  v: TValue;
  // I: Integer;
begin
  Result := False;
  obj := AObject;
  for field in AFieldsMapping do
  begin
    if CompareText(field.field_type, 'string') = 0 then
      v := AReader.Value[AReader.GetOrdinal(field.field)].AsString
    else if CompareText(field.field_type, 'integer') = 0 then
      v := AReader.Value[AReader.GetOrdinal(field.field)].AsInt32
    else if CompareText(field.field_type, 'date') = 0 then
    begin
      v := AReader.Value[AReader.GetOrdinal(field.field)].AsDate

    end
    else
      raise Exception.Create('Unknown field type for ' + field.field);
    TdormUtils.SetField(obj, field.name, v);
  end;
  Result := true;
end;

function TFirebirdPersistStrategy.RawExecute(SQL: string): Int64;
begin
  GetLogger.Warning('RAW EXECUTE: ' + SQL);
  Result := FB.Execute(SQL);
end;

function TFirebirdPersistStrategy.CreateObjectFromDBXReader
  (ARttiType: TRttiType; AReader: TDBXReader;
  AFieldsMapping: TArray<TdormFieldMapping>): TObject;
var
  obj: TObject;
  // obj1: TStringBuilder;
  field: TdormFieldMapping;
  v: TValue;
  t: TTimeStamp;
  S: string;
  sourceStream: TStream;
  targetStream: TMemoryStream;
begin
  Result := nil;
  try
    obj := TdormUtils.CreateObject(ARttiType);
    for field in AFieldsMapping do
    begin
      if CompareText(field.field_type, 'string') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.field)].AsString;
        S := field.field + ' as string';
      end
      else if CompareText(field.field_type, 'integer') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.field)].AsInt32;
        S := field.field + ' as integer';
      end
      else if CompareText(field.field_type, 'date') = 0 then
      begin
        t.Date := AReader.Value[AReader.GetOrdinal(field.field)].AsDate;
        t.Time := 0;
        v := TimeStampToDateTime(t);
        S := field.field + ' as date';
      end
      else if CompareText(field.field_type, 'blob') = 0 then
      begin
        targetStream := nil;
        sourceStream := AReader.Value[AReader.GetOrdinal(field.field)].AsStream;
        S := field.field + ' as blob';
        if assigned(sourceStream) then
        begin
          sourceStream.Position := 0;
          targetStream := TMemoryStream.Create;
          targetStream.CopyFrom(sourceStream, sourceStream.Size);
          targetStream.Position := 0;
        end;
        v := targetStream;
      end
      else if CompareText(field.field_type, 'decimal') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.field)].AsDouble;
        S := field.field + ' as decimal';
      end
      else if CompareText(field.field_type, 'boolean') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.field)].AsBoolean;
        S := field.field + ' as boolean';
      end
      else if CompareText(field.field_type, 'datetime') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.field)].AsDateTime;
        S := field.field + ' as datetime';
      end
      else
        raise Exception.Create('Unknown field type for ' + field.field);
      try
        TdormUtils.SetField(obj, field.name, v);
      except
        on E: Exception do
        begin
          raise EdormException.Create(E.Message + sLineBreak +
            '. Probably cannot read ' + ARttiType.ToString + '.' + S);
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

class procedure TFirebirdPersistStrategy.register;
begin
  //
end;

procedure TFirebirdPersistStrategy.Rollback;
begin
  if not assigned(FCurTransaction) then
    raise Exception.Create('Transaction is not active');
  FB.GetConnection.RollbackFreeAndNil(FCurTransaction);
end;

procedure TFirebirdPersistStrategy.SetDBXParameterValue(aFieldType: string;
  aParameter: TDBXParameter; aValue: TValue);
begin
  if CompareText(aFieldType, 'string') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.WideStringType;
  end
  else

    if CompareText(aFieldType, 'decimal') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.DoubleType;
  end
  else if CompareText(aFieldType, 'boolean') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.Int16Type;

  end
  else if CompareText(aFieldType, 'date') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.DateType;

  end
  else if CompareText(aFieldType, 'blob') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.BlobType;

  end;

  SetDBXValue(aFieldType, aParameter.Value, aValue);
end;

procedure TFirebirdPersistStrategy.SetDBXValue(aFieldType: string;
  aDBXValue: TDBXWritableValue; aValue: TValue);

var
  str: TBytesStream;
  sourceStream: TStream;
begin
  if CompareText(aFieldType, 'string') = 0 then
  begin
    aDBXValue.AsString := aValue.AsString;
  end
  else if CompareText(aFieldType, 'integer') = 0 then
  begin
    aDBXValue.AsBcd := IntegerToBcd(aValue.AsInt64);
  end
  else if CompareText(aFieldType, 'date') = 0 then
  begin
    aDBXValue.AsDate := DateTimeToTimeStamp(aValue.AsExtended).Date;
  end
  else if CompareText(aFieldType, 'datetime') = 0 then
  begin
    aDBXValue.AsDateTime := FloatToDateTime(aValue.AsExtended);
  end
  else if CompareText(aFieldType, 'blob') = 0 then
  begin
    str := TBytesStream.Create;
    sourceStream := TStream(aValue.AsObject);
    if sourceStream = nil then
      aDBXValue.SetNull
    else
    begin
      sourceStream.Position := 0;
      str.CopyFrom(sourceStream, sourceStream.Size);
      str.Position := 0;
      aDBXValue.SetStream(str, true);
      aDBXValue.ValueType.ValueTypeFlags :=
        aDBXValue.ValueType.ValueTypeFlags or TDBXValueTypeFlags.ExtendedType;
    end;
  end
  else if CompareText(aFieldType, 'decimal') = 0 then
  begin

    aDBXValue.AsDouble := aValue.AsExtended;

  end
  else if CompareText(aFieldType, 'boolean') = 0 then
  begin

    if aValue.AsBoolean then
      aDBXValue.AsInt16 := 1
    else
      aDBXValue.AsInt16 := 0;

  end
  else
    raise Exception.Create('Unsupported type ' + IntToStr(ord(aValue.Kind)));

end;

procedure TFirebirdPersistStrategy.SetLogger(ALogger: IdormLogger);
begin
  FLogger := ALogger;
end;

procedure TFirebirdPersistStrategy.StartTransaction;
begin
  if assigned(FCurTransaction) then
    raise Exception.Create('Transaction already active');
  FCurTransaction := FB.GetConnection.BeginTransaction
    (TDBXIsolations.ReadCommitted)
end;

{ TFirebirdTableSequence }

function TFirebirdTableSequence.NewIntegerKey(const Entity: string): UInt64;
var
  rdr: TDBXReader;
  cmd: TDBXCommand;
begin
  try
    cmd := FirebirdConnection.Prepare('SELECT GEN_ID(SEQ_' + Entity +
      '_ID, 1) FROM RDB$DATABASE');
    try
      rdr := cmd.ExecuteQuery;
      try
        if rdr.Next then
          Result := rdr.Value[0].AsInt64
        else
          raise EdormException.Create('Cannot get the key');
        rdr.Close;
      finally
        rdr.Free;
      end;
    finally
      cmd.Free;
    end;
  except
    on E: Exception do
    begin
      raise EdormException.Create('Cannot get the key: ' + E.Message);
    end;
  end;
end;

function TFirebirdTableSequence.NewStringKey(const Entity: string): string;
begin
  raise EdormException.Create('String keys not supported');
end;

class procedure TFirebirdTableSequence.RegisterClass;
begin
  // do nothing
end;

procedure TFirebirdTableSequence.SetFirebirdConnection(const Value: TFBFactory);
begin
  FFirebirdConnection := Value;
end;

initialization

TFirebirdPersistStrategy.register;
TFirebirdTableSequence.RegisterClass;

finalization

end.
