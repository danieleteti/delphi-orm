{ *******************************************************************************
  Copyright 2010-2013 Daniele Teti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ******************************************************************************** }

unit dorm.adapter.SQLServer;

interface

uses
  dorm.Commons,
  classes,
  SysUtils,
  DB,
  SqlExpr,
  dorm.adapter.DBExpress.Factory,
  DBXClient,
  DBXCommon,
  dbclient,
  Rtti,
  dorm,
  dorm.Mappings,
  superobject,
  TypInfo,
  FMTBcd,
  sqlTimSt,
  DBXMSSQL,
  dorm.Filters,
  Generics.Collections,
  dorm.Collections,
  dorm.adapter.Base;

type

  TSQLServerPersistStrategy = class(TBaseAdapter, IdormPersistStrategy)
  private
  strict protected
    FB: TDBXFactory;
    FLogger: IdormLogger;
    FCurTransaction: TDBXTransaction;
    FKeysGeneratorClassName: string;
    FKeysGenerator: IdormKeysGenerator;
    FLastInsertOID: TValue;
    procedure InitializeParamsForCommand(var Command: TDBXCommand;
      AFieldsMapping: TMappingFieldList); virtual;
    procedure InitializePKParams(var Command: TDBXCommand;
      AFieldsMapping: TMappingFieldList); virtual;
    function CreateDBXFactory(Conf: ISuperObject): TDBXFactory; virtual;
    function CreateObjectFromDBXReader(ARttiType: TRttiType;
      AReader: TDBXReader; AFieldsMapping: TMappingFieldList): TObject;
    function GetLogger: IdormLogger;
    procedure SetDBXParameterValue(aFieldType: string;
      aParameter: TDBXParameter; aValue: TValue);
    procedure SetDBXValue(aFieldType: string; aDBXValue: TDBXWritableValue;
      aValue: TValue);
  public
    function getGeneratedIdentity(): TValue; overload;
    function EscapeString(const Value: string): string;
    function EscapeDate(const Value: TDate): string;
    function EscapeDateTime(const Value: TDate): string;
    function GetLastInsertOID: TValue;
    function GetKeysGenerator: IdormKeysGenerator;
    function Insert(ARttiType: TRttiType; AObject: TObject; ATableName: string;
      AFieldsMapping: TMappingFieldList): TValue; virtual;
    function Update(ARttiType: TRttiType; AObject: TObject; ATableName: string;
      AFieldsMapping: TMappingFieldList): TValue;
    function Load(ARttiType: TRttiType; ATableName: string;
      AFieldsMapping: TMappingFieldList; const Value: TValue)
      : TObject; overload;

    function List(ARttiType: TRttiType; ATableName: string;
      AFieldsMapping: TMappingFieldList;
      AdormSearchCriteria: IdormSearchCriteria): TObjectList<TObject>;
    procedure FillList(AList: TObject; ARttiType: TRttiType;
      ATableName: string; AFieldsMapping: TMappingFieldList;
      AdormSearchCriteria: IdormSearchCriteria);

    function Delete(ARttiType: TRttiType; AObject: TObject; ATableName: string;
      AFieldsMapping: TMappingFieldList): TObject;
    procedure DeleteAll(ATableName: string);
    function Count(ATableName: string): Int64;
    procedure ConfigureStrategy(ConfigurationInfo: ISuperObject); virtual;
    procedure InitStrategy;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function InTransaction: Boolean;
    procedure SetLogger(ALogger: IdormLogger);
    destructor Destroy; override;
    class procedure register;
    function RawExecute(SQL: string): Int64;
    function ExecuteAndGetFirst(SQL: string): Int64;
  end;

  TSQLServerIdentity = class(TdormInterfacedObject, IdormKeysGenerator)
  private
    FPersistStrategy: IdormPersistStrategy;
  public
    function NewStringKey(const Entity: string): string;
    function NewIntegerKey(const Entity: string): UInt64;
    procedure SetPersistStrategy(const PersistentStrategy
      : IdormPersistStrategy);
    class procedure RegisterClass;
  end;

const
  LocalDateFormat = 'DD/MM/YYYY';
  LocalDateTimeFormat = 'DD/MM/YYYY HH:MM:SS';

implementation

uses
  dorm.Utils;

function TSQLServerPersistStrategy.Update(ARttiType: TRttiType; AObject: TObject;
  ATableName: string; AFieldsMapping: TMappingFieldList): TValue;
var
  field: TMappingField;
  SQL: string;
  Query: TDBXCommand;
  I, pk_idx: Integer;
  Value: TValue;
  sql_fields_names: string;
  pk_field: string;
begin
  sql_fields_names := '';
  for field in AFieldsMapping do
    if not field.IsPK then
      sql_fields_names := sql_fields_names + ',"' + field.FieldName + '" = ?';
  System.Delete(sql_fields_names, 1, 1);

  pk_field := AFieldsMapping[GetPKMappingIndex(AFieldsMapping)].FieldName;
  SQL := Format('UPDATE %S SET %S WHERE %S = ?', [ATableName, sql_fields_names,
    pk_field]);

  GetLogger.Debug(AFieldsMapping[GetPKMappingIndex(AFieldsMapping)].FieldName);

  GetLogger.Debug('PREPARING: ' + SQL);
  Query := FB.Prepare(SQL);
  InitializeParamsForCommand(Query, AFieldsMapping);
  try
    I := 0;
    for field in AFieldsMapping do
    begin
      Value := TdormUtils.GetField(AObject, field.name);

      if field.IsPK then
        Continue
      else
      begin
        SetDBXParameterValue(field.FieldType, Query.Parameters[I], Value);
      end;
      inc(I);
    end;
    pk_idx := GetPKMappingIndex(AFieldsMapping);
    InitializePKParams(Query, AFieldsMapping);
    Value := ARttiType.GetProperty(AFieldsMapping[pk_idx].name).GetValue(AObject);

    SetDBXParameterValue(AFieldsMapping[pk_idx].FieldType, Query.Parameters[I], Value);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    FB.Execute(Query);
  finally
    Query.Free;
  end;
end;

procedure TSQLServerPersistStrategy.Commit;
begin
  if not assigned(FCurTransaction) then
    raise Exception.Create('Transaction is not active');
  FB.GetConnection.CommitFreeAndNil(FCurTransaction);
end;

procedure TSQLServerPersistStrategy.ConfigureStrategy(ConfigurationInfo
  : ISuperObject);
var
  ctx: TRttiContext;
  t: TRttiType;
  obj: TObject;
begin
  FB := CreateDBXFactory(ConfigurationInfo);
  FKeysGeneratorClassName := ConfigurationInfo.S['keys_generator'];
  t := ctx.FindType(FKeysGeneratorClassName);
  if t = nil then
    raise EdormException.Create('Unknown key generator ' +
      FKeysGeneratorClassName);
  obj := t.AsInstance.MetaclassType.Create;
  if not Supports(obj, IdormKeysGenerator, FKeysGenerator) then
    raise EdormException.Create('Keys generator ' + FKeysGeneratorClassName +
      ' doesn''t implements ''IdormKeysGenerator''');
  FKeysGenerator.SetPersistStrategy(self);
  self._Release;
  inherited;
end;

function TSQLServerPersistStrategy.Count(ATableName: string): Int64;
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

function TSQLServerPersistStrategy.CreateDBXFactory(Conf: ISuperObject)
  : TDBXFactory;
begin
  Result := TDBXFactory.Create('dbxmss.dll', 'MSSQL', Conf);
end;

function TSQLServerPersistStrategy.Delete(ARttiType: TRttiType; AObject: TObject;
  ATableName: string; AFieldsMapping: TMappingFieldList): TObject;
var
  pk_idx: Integer;
  pk_value: TValue;
  pk_attribute_name, pk_field_name, SQL: string;
  cmd: TDBXCommand;
begin
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + ATableName);
  pk_attribute_name := AFieldsMapping[pk_idx].name;
  pk_field_name := AFieldsMapping[pk_idx].FieldName;
  pk_value := ARttiType.GetProperty(pk_attribute_name).GetValue(AObject);
  SQL := 'DELETE FROM ' + ATableName + ' WHERE ' + pk_field_name + ' = ?';
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);
  InitializePKParams(cmd, AFieldsMapping);
  try
    SetDBXParameterValue(AFieldsMapping[pk_idx].FieldType, cmd.Parameters[pk_idx], pk_value);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    cmd.ExecuteUpdate;
    Result := nil;
  finally
    cmd.Free;
  end;
end;

procedure TSQLServerPersistStrategy.DeleteAll(ATableName: string);
var
  SQL: string;
begin
  SQL := 'DELETE FROM ' + ATableName;
  GetLogger.Debug('EXECUTING :' + SQL);
  FB.Execute(SQL);
end;

destructor TSQLServerPersistStrategy.Destroy;
begin
  FB.Free;
  inherited;
end;

function TSQLServerPersistStrategy.EscapeDate(const Value: TDate): string;
begin
  Result := FormatDateTime(LocalDateFormat, Value, TFormatSettings.Create);
end;

function TSQLServerPersistStrategy.EscapeDateTime(const Value: TDate): string;
begin
  Result := FormatDateTime(LocalDateTimeFormat, Value, TFormatSettings.Create);
end;

function TSQLServerPersistStrategy.EscapeString(const Value: string): string;
begin
  Result := StringReplace(Value, '''', '''''', [rfReplaceAll]);
end;

function TSQLServerPersistStrategy.ExecuteAndGetFirst(SQL: string): Int64;
var
  rdr: TDBXReader;
  cmd: TDBXCommand;
begin
  Result := -1;
  cmd := FB.Prepare(SQL);
  try
    rdr := cmd.ExecuteQuery;
    try
      if rdr.Next then
        Result := rdr.Value[0].AsInt64
      else
        raise EdormException.Create('ExecuteAndGetFirst returns o rows');
      rdr.Close;
    finally
      rdr.Free;
    end;
  finally
    cmd.Free;
  end;
end;

function TSQLServerPersistStrategy.getGeneratedIdentity(): TValue;
begin
  Result := Int64(FKeysGenerator.NewIntegerKey(''));
  FLastInsertOID := Result;
end;

function TSQLServerPersistStrategy.GetKeysGenerator: IdormKeysGenerator;
begin
  Result := FKeysGenerator;
end;

function TSQLServerPersistStrategy.GetLastInsertOID: TValue;
begin
  Result := FLastInsertOID;
end;

function TSQLServerPersistStrategy.GetLogger: IdormLogger;
begin
  Result := FLogger;
end;

procedure TSQLServerPersistStrategy.InitializeParamsForCommand(
  var Command: TDBXCommand; AFieldsMapping: TMappingFieldList);
begin
  // Do nothing;
end;

procedure TSQLServerPersistStrategy.InitializePKParams(var Command: TDBXCommand;
  AFieldsMapping: TMappingFieldList);
begin
  // Do Nothing
end;

procedure TSQLServerPersistStrategy.InitStrategy;
begin
  FLastInsertOID := TValue.Empty;
end;

function TSQLServerPersistStrategy.Insert(ARttiType: TRttiType; AObject: TObject;
  ATableName: string; AFieldsMapping: TMappingFieldList): TValue;
var
  field: TMappingField;
  sql_fields_names, sql_fields_values: string;
  SQL, outVal: string;
  Query: TDBXCommand;
  rdr: TDBXReader;
  I, pk_idx: Integer;
  v, pk_value: TValue;
begin
  outVal := '';
  sql_fields_names := '';
  for field in AFieldsMapping do
  begin
    if not field.IsPK then
      sql_fields_names := sql_fields_names + ',"' + field.FieldName + '"';
  end;

  System.Delete(sql_fields_names, 1, 1);

  sql_fields_values := '';
  for field in AFieldsMapping do
  begin
    if not field.IsPK then
    begin
      sql_fields_values := sql_fields_values + ',?';
    end
    else
    begin
      if CompareText(field.FieldType, 'uniqueidentifier') = 0 then
        outVal := ' OUTPUT Inserted.' + field.FieldName;
    end;
  end;
  System.Delete(sql_fields_values, 1, 1);

  SQL := Format('INSERT INTO %s (%S) %s VALUES (%S)',
    [ATableName, sql_fields_names, outVal, sql_fields_values]);

  // Add Support to UNIQUEIDENTIFIER PKs (default value = newsequentialid())

  GetLogger.Debug('PREPARING :' + SQL);
  Query := FB.Prepare(SQL);
  InitializeParamsForCommand(Query, AFieldsMapping);
  try
    I := 0;
    for field in AFieldsMapping do
    begin
      v := TdormUtils.GetField(AObject, field.name);
      if not field.IsPK then
      begin
        SetDBXParameterValue(field.FieldType, Query.Parameters[I], v);
        inc(I);
      end;
    end;
    GetLogger.Debug('EXECUTING PREPARED :' + string(SQL));
    rdr := Query.ExecuteQuery;
    for field in AFieldsMapping do
    begin
      if (field.IsPK) then
      begin
        if (field.FieldType = 'integer') then
        begin
          pk_value := getGeneratedIdentity();
        end
        else
        begin
          try
            if rdr.Next then
              pk_value := rdr.Value[0].AsString
            else
              raise EdormException.Create('Could not obtain uniqueidentifier');
            rdr.Close;
          finally
            rdr.Free;
          end;
        end;
      end;
    end;
  finally
    Query.Free;
  end;
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  TdormUtils.SetProperty(AObject, AFieldsMapping[pk_idx].name, pk_value);
  Result := pk_value;
end;

function TSQLServerPersistStrategy.InTransaction: Boolean;
begin
  Result := assigned(FCurTransaction);
end;

function TSQLServerPersistStrategy.List(ARttiType: TRttiType; ATableName: string;
  AFieldsMapping: TMappingFieldList;
  AdormSearchCriteria: IdormSearchCriteria): TObjectList<TObject>;
begin
  Result := NewList();
  FillList(Result, ARttiType, ATableName, AFieldsMapping, AdormSearchCriteria);
end;

procedure TSQLServerPersistStrategy.FillList(AList: TObject;
  ARttiType: TRttiType; ATableName: string;
  AFieldsMapping: TMappingFieldList;
  AdormSearchCriteria: IdormSearchCriteria);
var
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
      while reader.Next do
        TdormUtils.MethodCall(AList, 'Add', [CreateObjectFromDBXReader(ARttiType, reader,
          AFieldsMapping)]);
    finally
      reader.Free;
    end;
  finally
    cmd.Free;
  end;
end;

function TSQLServerPersistStrategy.Load(ARttiType: TRttiType; ATableName: string;
  AFieldsMapping: TMappingFieldList; const Value: TValue): TObject;
var
  pk_idx: Integer;
  pk_attribute_name, pk_field_name, SQL: string;
  cmd: TDBXCommand;
  reader: TDBXReader;
begin
  Result := nil;
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + ATableName);
  pk_attribute_name := AFieldsMapping[pk_idx].name;
  pk_field_name := AFieldsMapping[pk_idx].FieldName;
  SQL := 'SELECT ' + GetSelectFieldsList(AFieldsMapping, true) + ' FROM ' +
    ATableName + ' WHERE ' + pk_field_name + ' = ?';
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);
  InitializePKParams(cmd, AFieldsMapping);
  try
    SetDBXParameterValue(AFieldsMapping[pk_idx].FieldType, cmd.Parameters[pk_idx], Value);
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

function TSQLServerPersistStrategy.RawExecute(SQL: string): Int64;
begin
  GetLogger.Warning('RAW EXECUTE: ' + SQL);
  Result := FB.Execute(SQL);
end;

function TSQLServerPersistStrategy.CreateObjectFromDBXReader
  (ARttiType: TRttiType; AReader: TDBXReader;
  AFieldsMapping: TMappingFieldList): TObject;
var
  obj: TObject;
  field: TMappingField;
  v: TValue;
  t: TTimeStamp;
  S: string;
  sourceStream: TStream;
  targetStream: TMemoryStream;
begin
  try
    obj := TdormUtils.CreateObject(ARttiType);
    for field in AFieldsMapping do
    begin
      if CompareText(field.FieldType, 'string') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.FieldName)].AsString;
        S := field.FieldName + ' as string';
      end
      else if CompareText(field.FieldType, 'uniqueidentifier') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.FieldName)].AsString;
        S := field.FieldName + ' as string';
      end
      else if CompareText(field.FieldType, 'integer') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.FieldName)].AsInt32;
        S := field.FieldName + ' as integer';
      end
      else if CompareText(field.FieldType, 'date') = 0 then
      begin
        t.Date := AReader.Value[AReader.GetOrdinal(field.FieldName)].AsDate;
        t.Time := 0;
        v := TimeStampToDateTime(t);
        S := field.FieldName + ' as date';
      end
      else if CompareText(field.FieldType, 'blob') = 0 then
      begin
        targetStream := nil;
        sourceStream := AReader.Value[AReader.GetOrdinal(field.FieldName)].AsStream;
        S := field.FieldName + ' as blob';
        if assigned(sourceStream) then
        begin
          sourceStream.Position := 0;
          targetStream := TMemoryStream.Create;
          targetStream.CopyFrom(sourceStream, sourceStream.Size);
          targetStream.Position := 0;
        end;
        v := targetStream;
      end
      else if CompareText(field.FieldType, 'decimal') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.FieldName)].AsDouble;
        S := field.FieldName + ' as decimal';
      end
      else if CompareText(field.FieldType, 'boolean') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.FieldName)].AsBoolean;
        S := field.FieldName + ' as boolean';
      end
      else if CompareText(field.FieldType, 'datetime') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.FieldName)].AsDateTime;
        S := field.FieldName + ' as datetime';
      end
      else if CompareText(field.FieldType, 'timestamp') = 0 then
      begin
        v := AReader.Value[AReader.GetOrdinal(field.FieldName)].AsDateTime;
        S := field.FieldName + ' as datetime';
      end
      else
        raise Exception.Create('Unknown field type for ' + field.FieldName);
      try
        TdormUtils.SetField(obj, field.name, v);
      except
        on E: Exception do
        begin

          raise EdormException.Create(E.Message + sLineBreak +
            '. Probably cannot write ' + ARttiType.ToString + '.' + S);
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

class procedure TSQLServerPersistStrategy.register;
begin
  //
end;

procedure TSQLServerPersistStrategy.Rollback;
begin
  if not assigned(FCurTransaction) then
    raise Exception.Create('Transaction is not active');
  FB.GetConnection.RollbackFreeAndNil(FCurTransaction);
end;

procedure TSQLServerPersistStrategy.SetDBXParameterValue(aFieldType: string;
  aParameter: TDBXParameter; aValue: TValue);
begin
  if CompareText(aFieldType, 'string') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.WideStringType;
  end
  else if CompareText(aFieldType, 'uniqueidentifier') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.WideStringType;
  end
  else if CompareText(aFieldType, 'integer') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.Int32Type;
  end
  else if CompareText(aFieldType, 'decimal') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.DoubleType;
  end
  else if CompareText(aFieldType, 'boolean') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.Int16Type;
  end
  else if CompareText(aFieldType, 'datetime') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.TimeStampType;
  end
  else if CompareText(aFieldType, 'date') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.TimeStampType;
  end
  else if CompareText(aFieldType, 'timestamp') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.TimeStampType;
  end
  else if CompareText(aFieldType, 'blob') = 0 then
  begin
    aParameter.DataType := TDBXDataTypes.BlobType;
  end;

  SetDBXValue(aFieldType, aParameter.Value, aValue);
end;

procedure TSQLServerPersistStrategy.SetDBXValue(aFieldType: string;
  aDBXValue: TDBXWritableValue; aValue: TValue);

var
  str: TBytesStream;
  sourceStream: TStream;
begin
  if CompareText(aFieldType, 'string') = 0 then
  begin
    aDBXValue.AsString := aValue.AsString;
  end
  else if CompareText(aFieldType, 'uniqueidentifier') = 0 then
  begin
    aDBXValue.AsString := aValue.AsString;
  end
  else if CompareText(aFieldType, 'integer') = 0 then
  begin
    aDBXValue.AsInt32 := aValue.AsInteger;
  end
  else if CompareText(aFieldType, 'datetime') = 0 then
  begin
    aDBXValue.AsTimeStamp := DateTimeToSQLTimeStamp(FloatToDateTime(aValue.AsExtended));
  end
  else if CompareText(aFieldType, 'date') = 0 then
  begin
    aDBXValue.AsTimeStamp := DateTimeToSQLTimeStamp(FloatToDateTime(aValue.AsExtended));
  end
  else if CompareText(aFieldType, 'timestamp') = 0 then
  begin
    aDBXValue.AsTimeStamp := DateTimeToSQLTimeStamp(FloatToDateTime(aValue.AsExtended));
  end

  else if CompareText(aFieldType, 'blob') = 0 then
  begin
    sourceStream := TStream(aValue.AsObject);
    if sourceStream = nil then
      aDBXValue.SetNull
    else
    begin
      str := TBytesStream.Create;
      try
        sourceStream.Position := 0;
        str.CopyFrom(sourceStream, sourceStream.Size);
        str.Position := 0;
        aDBXValue.SetStream(str, true);
        aDBXValue.ValueType.ValueTypeFlags :=
          aDBXValue.ValueType.ValueTypeFlags or TDBXValueTypeFlags.ExtendedType;
      except
        str.Free;
        raise;
      end;
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
    raise Exception.Create('Unsupported type ' + inttostr(ord(aValue.Kind)));

end;

procedure TSQLServerPersistStrategy.SetLogger(ALogger: IdormLogger);
begin
  FLogger := ALogger;
end;

procedure TSQLServerPersistStrategy.StartTransaction;
begin
  if assigned(FCurTransaction) then
    raise Exception.Create('Transaction already active');
  FCurTransaction := FB.GetConnection.BeginTransaction
    (TDBXIsolations.ReadCommitted)
end;

{ TFirebirdTableSequence }

function TSQLServerIdentity.NewIntegerKey(const Entity: string): UInt64;
begin
  Result := FPersistStrategy.ExecuteAndGetFirst('SELECT @@IDENTITY');
end;

function TSQLServerIdentity.NewStringKey(const Entity: string): string;
begin
  raise EdormException.Create('String keys not supported');
end;

class procedure TSQLServerIdentity.RegisterClass;
begin
  // do nothing
end;

procedure TSQLServerIdentity.SetPersistStrategy(const PersistentStrategy
  : IdormPersistStrategy);
begin
  FPersistStrategy := PersistentStrategy;
end;

initialization

TSQLServerPersistStrategy.register;
TSQLServerIdentity.RegisterClass;

finalization

end.
