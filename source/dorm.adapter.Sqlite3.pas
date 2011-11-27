{ *******************************************************************************
  Copyright 2010-2011 Daniele Teti

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

unit dorm.adapter.Sqlite3;

interface

uses
  dorm.Commons,
  classes,
  SysUtils,
  DB,
  Sqlite3,
  Sqlite3udf,
  SQLiteWrap,
  //dorm.adapter.DBExpress.Factory,
  Rtti,
  dorm,
  superobject,
  TypInfo,
  FMTBcd,
  dorm.Collections;

type
  TSqlite3PersistStrategy = class(TdormInterfacedObject, IdormPersistStrategy)
  strict protected
    FFormatSettings: TFormatSettings;
    DB: TSQLiteDatabase;
    FLogger: IdormLogger;
    // FKeysGeneratorClassName: string;  //not supported
    // FKeysGenerator: IdormKeysGenerator; //not supported
    FKeyType: TdormKeyType;
    FNullKeyValue: TValue;
    FLastInsertOID: TValue;
    procedure InitFormatSettings;
    function CreateObjectFromSqliteTable(ARttiType: TRttiType;
      AReader: TSqliteTable; AFieldsMapping: TArray<TdormFieldMapping>)
      : TObject;
    function GetLogger: IdormLogger;
    procedure SetSqlite3ParameterValue(ADB: TSQLiteDatabase;
      aFieldType: string;
      aParameterName: String; aValue: TValue);
  public
    function FillPrimaryKeyParam(ADB: TSQLiteDatabase;
      AParamName: String; const Value: TValue): TValue; overload;
    function EscapeString(const Value: String): String;
    function EscapeDate(const Value: TDate): String;
    function EscapeDateTime(const Value: TDate): String;
    function GetLastInsertOID: TValue;
    function GetKeysGenerator: IdormKeysGenerator;
    function Insert(ARttiType: TRttiType; AObject: TObject; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>): TValue;
    function Update(ARttiType: TRttiType; AObject: TObject; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>): TValue;
    function Load(ARttiType: TRttiType; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>; const Value: TValue)
      : TObject; overload;

    function List(ARttiType: TRttiType; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>;
      AdormSearchCriteria: IdormSearchCriteria): TdormCollection;
    procedure FillList(AList: TObject; ARttiType: TRttiType;
      ATableName: string; AFieldsMapping: TArray<TdormFieldMapping>;
      AdormSearchCriteria: IdormSearchCriteria);

    function Delete(ARttiType: TRttiType; AObject: TObject; ATableName: string;
      AFieldsMapping: TArray<TdormFieldMapping>): TObject;
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
    function IsNullKey(const Value: TValue): Boolean;
    function GetNullKeyValue: TValue;
    function GetKeyType: TdormKeyType;
    function RawExecute(SQL: string): Int64;
    function ExecuteAndGetFirst(SQL: string): Int64;
  end;

  TSqlite3TableSequence = class(TdormInterfacedObject, IdormKeysGenerator)
  private
    FPersistStrategy: IdormPersistStrategy;
  public
    function NewStringKey(const Entity: string): string;
    function NewIntegerKey(const Entity: string): UInt64;
    procedure SetPersistStrategy(const PersistentStrategy
      : IdormPersistStrategy);
    class procedure RegisterClass;
  end;

implementation

uses
  dorm.Utils, CodeSiteLogging;

function TSqlite3PersistStrategy.Update(ARttiType: TRttiType;
  AObject: TObject;
  ATableName: string; AFieldsMapping: TArray<TdormFieldMapping>): TValue;
var
  field: TdormFieldMapping;
  SQL: string;
  //I, pk_idx: Integer;
  v: TValue;
  sql_fields_names: string;
  pk_field: string;
begin
  sql_fields_names := '';
  for field in AFieldsMapping do
    if not field.pk then
      sql_fields_names := sql_fields_names + ',"' + field.field + '" = :' +
        field.field;
  System.Delete(sql_fields_names, 1, 1);

  pk_field := AFieldsMapping[GetPKMappingIndex(AFieldsMapping)].field;
  SQL := Format('UPDATE %S SET %S WHERE "%S" = :%S',
    [ATableName, sql_fields_names,
    pk_field, pk_field]);

  GetLogger.Debug(AFieldsMapping[GetPKMappingIndex(AFieldsMapping)].field);
  DB.ParamsClear;
  GetLogger.Debug('PREPARING: ' + SQL);
  for field in AFieldsMapping do
  begin
    v := TdormUtils.GetField(AObject, field.name);
    SetSqlite3ParameterValue(DB, field.field_type, ':' + field.field, v);
  end;
  GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
  DB.ExecSQL(SQL);
  DB.ParamsClear;
end;

procedure TSqlite3PersistStrategy.Commit;
begin
  if not DB.InTransaction then
    raise Exception.Create('Transaction is not active');
  DB.Commit('trans');
end;

procedure TSqlite3PersistStrategy.ConfigureStrategy(ConfigurationInfo
  : ISuperObject);
var
//  ctx: TRttiContext;
//  t: TRttiType;
//  obj: TObject;
  database_connection_string: string;
  // password: string; //Not supported
begin
  InitFormatSettings;
  database_connection_string := ConfigurationInfo.S
    ['database_connection_string'];
  // Do not support password. The configuration about "password" is ingored
  // password := ConfigurationInfo.S['password'];
  DB := TSQLiteDatabase.Create(database_connection_string);
  // Do not support String Keys Generator. The configuration about "keys_generator" is ingored
  // FKeysGeneratorClassName := ConfigurationInfo.S['keys_generator'];
  // t := ctx.FindType(FKeysGeneratorClassName);
  // if t = nil then
  // raise EdormException.Create('Unknown key generator ' +
  // FKeysGeneratorClassName);
  // obj := t.AsInstance.MetaclassType.Create;
  // if not Supports(obj, IdormKeysGenerator, FKeysGenerator) then
  // raise EdormException.Create('Keys generator ' + FKeysGeneratorClassName +
  // ' doesn''t implements ''IdormKeysGenerator''');
  // FKeysGenerator.SetPersistStrategy(self);
  // self._Release;

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

function TSqlite3PersistStrategy.Count(ATableName: string): Int64;
var
  SQL: string;
  Table: TSqliteTable;
begin
//  Result := -1;
  SQL := 'SELECT COUNT(*) FROM ' + ATableName;
  GetLogger.Debug('PREPARING: ' + SQL);
  Table := DB.GetTable(SQL);
  try
    Result := Table.FieldAsInteger(0);
  finally
    Table.Free;
  end;
end;

function TSqlite3PersistStrategy.Delete(ARttiType: TRttiType;
  AObject: TObject;
  ATableName: string; AFieldsMapping: TArray<TdormFieldMapping>): TObject;
var
  pk_idx: Integer;
  pk_value: TValue;
  pk_attribute_name, pk_field_name, SQL: string;
begin
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + ATableName);
  pk_attribute_name := AFieldsMapping[pk_idx].name;
  pk_field_name := AFieldsMapping[pk_idx].field;
  pk_value := ARttiType.GetProperty(pk_attribute_name).GetValue(AObject);
  SQL := 'DELETE FROM ' + AnsiQuotedStr(ATableName, '"') + ' WHERE ' +
    AnsiQuotedStr(pk_field_name, '"') + ' = :' +
    pk_field_name;
  GetLogger.Debug('PREPARING: ' + SQL);
  DB.ParamsClear;
  FillPrimaryKeyParam(DB, ':' + pk_field_name, pk_value);
  GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
  DB.ExecSQL(SQL);
  DB.ParamsClear;
  Result := nil;
end;

procedure TSqlite3PersistStrategy.DeleteAll(ATableName: string);
var
  SQL: string;
begin
  SQL := 'DELETE FROM ' + AnsiQuotedStr(ATableName, '"');
  GetLogger.Debug('EXECUTING :' + SQL);
  DB.ExecSQL(SQL);
end;

destructor TSqlite3PersistStrategy.Destroy;
begin
  DB.Free;
  inherited;
end;

function TSqlite3PersistStrategy.EscapeDate(const Value: TDate): String;
begin
  Result := FormatDateTime('YYYY-MM-DD', Value);
end;

function TSqlite3PersistStrategy.EscapeDateTime(const Value: TDate)
  : String;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS', Value);
end;

function TSqlite3PersistStrategy.EscapeString(const Value: String): String;
begin
  Result := StringReplace(Value, '''', '''''', [rfReplaceAll]);
end;

function TSqlite3PersistStrategy.ExecuteAndGetFirst(SQL: string): Int64;
begin
  Result := DB.GetTableValue(SQL);
end;

// function TSqlite3PersistStrategy.GenerateAndFillPrimaryKeyParam
// (PKParam: TDBXParameter; const Entity: string): TValue;
// begin
// case FKeyType of
// ktString:
// begin
// PKParam.Value.SetString(FKeysGenerator.NewStringKey(Entity));
// Result := PKParam.Value.GetString;
// end;
// ktInteger:
// begin
// PKParam.Value.AsInt64 := FKeysGenerator.NewIntegerKey(Entity);
// Result := PKParam.Value.AsInt64;
// end;
// end;
// FLastInsertOID := Result;
// end;

// function TSqlite3PersistStrategy.FillPrimaryKeyParam(ATable: TSqliteTable;
// AParamName: String;
// const Value: TValue): TValue;
// begin
// try
// case FKeyType of
// ktString:
// begin
// { todo: implement string primary keys for sqlite3 }
// raise EdormException.Create
// (ClassName + ' do not support string primary keys');
// // ATable.AddParamText(AParamName, Value.AsString);
// // Result := Value.AsString;
// end;
// ktInteger:
// begin
// ATable.AddParamInt(AParamName, Value.AsInteger);
// Result := Value.AsInteger;
// end;
// end;
// except
// on E: Exception do
// raise EdormException.Create('Error during fill primary key for query. ' +
// E.Message);
// end;
// end;

function TSqlite3PersistStrategy.GetKeysGenerator: IdormKeysGenerator;
begin
  // Result := FKeysGenerator;
  raise EdormException.Create('Not supported on ' + ClassName);
end;

function TSqlite3PersistStrategy.GetKeyType: TdormKeyType;
begin
  Result := FKeyType;
end;

function TSqlite3PersistStrategy.GetLastInsertOID: TValue;
begin
  Result := FLastInsertOID;
end;

function TSqlite3PersistStrategy.GetLogger: IdormLogger;
begin
  Result := FLogger;
end;

procedure TSqlite3PersistStrategy.InitFormatSettings;
begin
  FFormatSettings.LongDateFormat := 'YYYY-MM-DD';
  FFormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  FFormatSettings.LongTimeFormat := 'HH:NN:SS';
  FFormatSettings.ShortTimeFormat := 'HH:NN:SS';
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.TimeSeparator := ':';
end;

procedure TSqlite3PersistStrategy.InitStrategy;
begin
  FLastInsertOID := TValue.Empty;
end;

function TSqlite3PersistStrategy.Insert(ARttiType: TRttiType;
  AObject: TObject;
  ATableName: string; AFieldsMapping: TArray<TdormFieldMapping>): TValue;
var
  field: TdormFieldMapping;
  sql_fields_names, sql_fields_values, SQL: ansistring;
  {Param, I,} pk_idx: Integer;
  v, pk_value: TValue;
//  Query: TSqliteTable;
begin
  sql_fields_names := '';
  for field in AFieldsMapping do
    if not field.pk then
      sql_fields_names := sql_fields_names + ',' + AnsiString(field.field) + '';

  System.Delete(sql_fields_names, 1, 1);
//  Param := 0;
  sql_fields_values := '';
  for field in AFieldsMapping do
    if not field.pk then
      sql_fields_values := sql_fields_values + ', :' + AnsiString(field.field);
  System.Delete(sql_fields_values, 1, 1);

  SQL := AnsiString(Format('INSERT INTO %s (%S) VALUES (%S)',
    [ATableName, sql_fields_names, sql_fields_values]));
  GetLogger.Debug('PREPARING :' + string(SQL));

  DB.ParamsClear;
//  I := 1;
  for field in AFieldsMapping do
  begin
    v := TdormUtils.GetField(AObject, field.name);
    if not field.pk then
      SetSqlite3ParameterValue(DB, field.field_type, ':' + field.field, v);
  end;
  GetLogger.Debug('EXECUTING PREPARED :' + string(SQL));
  DB.ExecSQL(string(SQL));
  pk_value := DB.LastInsertRowID;
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  TdormUtils.SetProperty(AObject, AFieldsMapping[pk_idx].name, pk_value);
  Result := pk_value;
  FLastInsertOID := Result;
end;

function TSqlite3PersistStrategy.InTransaction: Boolean;
begin
  Result := DB.InTransaction;
end;

function TSqlite3PersistStrategy.IsNullKey(const Value: TValue): Boolean;
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

function TSqlite3PersistStrategy.GetNullKeyValue: TValue;
begin
  Result := FNullKeyValue;
end;

function TSqlite3PersistStrategy.List(ARttiType: TRttiType;
  ATableName: string;
  AFieldsMapping: TArray<TdormFieldMapping>;
  AdormSearchCriteria: IdormSearchCriteria): TdormCollection;
begin
  Result := NewList();
  FillList(Result, ARttiType, ATableName, AFieldsMapping,
    AdormSearchCriteria);
end;

procedure TSqlite3PersistStrategy.FillList(AList: TObject;
  ARttiType: TRttiType; ATableName: string;
  AFieldsMapping: TArray<TdormFieldMapping>;
  AdormSearchCriteria: IdormSearchCriteria);
var
  SQL: string;
  reader: TSqliteTable;
begin
  SQL := AdormSearchCriteria.GetSQL;
  GetLogger.Debug('EXECUTING: ' + SQL);
  reader := DB.GetTable(SQL, false);
  try
    while not reader.EOF do
    begin
      TdormUtils.MethodCall(AList, 'Add',
        [CreateObjectFromSqliteTable(ARttiType, reader,
        AFieldsMapping)]);
      reader.Next;
    end;
  finally
    reader.Free;
  end;
end;

function TSqlite3PersistStrategy.FillPrimaryKeyParam(ADB: TSQLiteDatabase;
  AParamName: String; const Value: TValue): TValue;
begin
  try
    case FKeyType of
      ktString:
        begin
          { todo: implement string primary keys for sqlite3 }
          raise EdormException.Create
            (ClassName + ' do not support string primary keys');
          // ADB.AddParamText(AParamName, Value.AsString);
          // Result := Value.AsString;
        end;
      ktInteger:
        begin
          ADB.AddParamInt(AParamName, Value.AsInteger);
          Result := Value.AsInteger;
        end;
    end;
  except
    on E: Exception do
      raise EdormException.Create('Error during fill primary key for query. ' +
        E.Message);
  end;
end;

function TSqlite3PersistStrategy.Load(ARttiType: TRttiType;
  ATableName: string;
  AFieldsMapping: TArray<TdormFieldMapping>; const Value: TValue): TObject;
var
  pk_idx: Integer;
  pk_attribute_name, pk_field_name, SQL: string;
  reader: TSqliteTable;
begin
  Result := nil;
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + ATableName);
  pk_attribute_name := AFieldsMapping[pk_idx].name;
  pk_field_name := AFieldsMapping[pk_idx].field;
  SQL := 'SELECT ' + GetSelectFieldsList(AFieldsMapping, true) + ' FROM ' +
    ATableName + ' WHERE ' + pk_field_name + ' = :' + pk_field_name;
  GetLogger.Debug('PREPARING: ' + SQL);
  DB.ParamsClear;
  FillPrimaryKeyParam(DB, ':' + pk_field_name, Value);
  GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
  reader := DB.GetTable(SQL);
  try
    if not reader.EOF then
      Result := CreateObjectFromSqliteTable(ARttiType, reader, AFieldsMapping);
  finally
    reader.Free;
  end;
end;

// function TSqlite3PersistStrategy.LoadObjectFromDBXReader(ARttiType: TRttiType;
// AReader: TDBXReader; AFieldsMapping: TArray<TdormFieldMapping>;
// AObject: TObject): Boolean;
// var
// obj: TObject;
// field: TdormFieldMapping;
// v: TValue;
// begin
// Result := False;
// obj := AObject;
// for field in AFieldsMapping do
// begin
// if CompareText(field.field_type, 'string') = 0 then
// v := AReader.Value[AReader.GetOrdinal(field.field)].AsString
// else if CompareText(field.field_type, 'integer') = 0 then
// v := AReader.Value[AReader.GetOrdinal(field.field)].AsInt32
// else if CompareText(field.field_type, 'date') = 0 then
// begin
// v := AReader.Value[AReader.GetOrdinal(field.field)].AsDate
//
// end
// else
// raise Exception.Create('Unknown field type for ' + field.field);
// TdormUtils.SetField(obj, field.name, v);
// end;
// Result := true;
// end;

function TSqlite3PersistStrategy.RawExecute(SQL: string): Int64;
begin
  GetLogger.Warning('RAW EXECUTE: ' + SQL);
  Result := 0;
  DB.ExecSQL(SQL); // sqlite3 do not return affected rows?
end;

function TSqlite3PersistStrategy.CreateObjectFromSqliteTable
  (ARttiType: TRttiType; AReader: TSqliteTable;
  AFieldsMapping: TArray<TdormFieldMapping>): TObject;
var
  obj: TObject;
  field: TdormFieldMapping;
  v: TValue;
  S: string;
  sourceStream: TStream;
//  targetStream: TMemoryStream;
begin
//  Result := nil;
  try
    obj := TdormUtils.CreateObject(ARttiType);
    for field in AFieldsMapping do
    begin
      if CompareText(field.field_type, 'string') = 0 then
      begin
        v := AReader.FieldAsString(AReader.FieldIndex[field.field]);
        S := field.field + ' as string';
      end
      else if CompareText(field.field_type, 'integer') = 0 then
      begin
        v := AReader.FieldAsInteger(AReader.FieldIndex[field.field]);
        S := field.field + ' as integer';
      end
      else if CompareText(field.field_type, 'date') = 0 then
      begin
        v := AReader.FieldAsString(AReader.FieldIndex[field.field]);
        v := StrToDate(v.AsString, FFormatSettings);
        S := field.field + ' as date';
      end
      else if CompareText(field.field_type, 'blob') = 0 then
      begin
//        targetStream := nil;
        sourceStream := nil;
        if not AReader.FieldIsNull(AReader.FieldIndex[field.field]) then
          sourceStream := AReader.FieldAsBlob(AReader.FieldIndex[field.field]);
        S := field.field + ' as blob';
        if assigned(sourceStream) then
        begin
          sourceStream.Position := 0;
          v := sourceStream;
        end
        else
          v := nil;
      end
      else if CompareText(field.field_type, 'decimal') = 0 then
      begin
        v := AReader.FieldAsDouble(AReader.FieldIndex[field.field]);
        S := field.field + ' as decimal';
      end
      else if CompareText(field.field_type, 'boolean') = 0 then
      begin
        v := AReader.FieldAsInteger(AReader.FieldIndex[field.field]);
        v := v.AsInteger = 1;
        S := field.field + ' as boolean';
      end
      else if CompareText(field.field_type, 'datetime') = 0 then
      begin
        v := AReader.FieldAsString(AReader.FieldIndex[field.field]);
        v := StrToDateTime(v.AsString, FFormatSettings);
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

class
  procedure TSqlite3PersistStrategy.register;
begin
  //
end;

procedure TSqlite3PersistStrategy.Rollback;
begin
  if not DB.InTransaction then
    raise Exception.Create('Transaction is not active');
  DB.Rollback('trans');
end;

procedure TSqlite3PersistStrategy.SetSqlite3ParameterValue
  (ADB: TSQLiteDatabase; aFieldType: string; aParameterName: String;
  aValue: TValue);
var
  sourceStream: TStream;
  str: TMemoryStream;
begin
  if CompareText(aFieldType, 'string') = 0 then
  begin
    ADB.AddParamText(aParameterName, aValue.AsString);
    GetLogger.Debug(aParameterName + ' = ' + aValue.AsString);
  end
  else if CompareText(aFieldType, 'integer') = 0 then
  begin
    ADB.AddParamInt(aParameterName, aValue.AsInteger);
    GetLogger.Debug(aParameterName + ' = ' + IntToStr(aValue.AsInteger));
  end
  else if CompareText(aFieldType, 'decimal') = 0 then
  begin
    ADB.AddParamFloat(aParameterName, aValue.AsExtended);
    GetLogger.Debug(aParameterName + ' = ' + FloatToStr(aValue.AsExtended));
  end
  else if CompareText(aFieldType, 'boolean') = 0 then
  begin
    if aValue.AsBoolean then
    begin
      ADB.AddParamInt(aParameterName, 1);
      GetLogger.Debug(aParameterName + ' = 1');
    end
    else
    begin
      ADB.AddParamInt(aParameterName, 0);
      GetLogger.Debug(aParameterName + ' = 0');
    end;
  end
  else if CompareText(aFieldType, 'date') = 0 then
  begin
    ADB.AddParamText(aParameterName,
      EscapeDate(aValue.AsExtended));
    GetLogger.Debug(aParameterName + ' = ' +
      EscapeDate(DateTimeToTimeStamp(aValue.AsExtended).Date));
  end
  else if CompareText(aFieldType, 'datetime') = 0 then
  begin
    ADB.AddParamText(aParameterName,
      EscapeDateTime(FloatToDateTime(aValue.AsExtended)));
    GetLogger.Debug(aParameterName + ' = ' +
      EscapeDateTime(FloatToDateTime(aValue.AsExtended)));
  end
  else if CompareText(aFieldType, 'blob') = 0 then
  begin
    sourceStream := TStream(aValue.AsObject);
    if sourceStream = nil then
    begin
      ADB.AddParamNull(aParameterName);
      GetLogger.Debug(aParameterName + ' = NULL');
    end
    else
    begin
      str := TMemoryStream.Create;
      try
        sourceStream.Position := 0;
        str.CopyFrom(sourceStream, sourceStream.Size);
        str.Position := 0;
        ADB.AddParamBlobPtr(aParameterName, str.Memory, str.Size);
        GetLogger.Debug(aParameterName + ' = <' + IntToStr(str.Size) +
          ' bytes>');
      finally
        str.Free;
      end;
    end;
  end;
end;

// procedure TSqlite3PersistStrategy.SetDBXValue(aFieldType: string;
// aDBXValue: TDBXWritableValue; aValue: TValue);
//
// var
// str: TBytesStream;
// sourceStream: TStream;
// begin
// if CompareText(aFieldType, 'string') = 0 then
// begin
// aDBXValue.AsString := aValue.AsString;
// end
// else if CompareText(aFieldType, 'integer') = 0 then
// begin
// aDBXValue.AsBcd := IntegerToBcd(aValue.AsInt64);
// end
// else if CompareText(aFieldType, 'date') = 0 then
// begin
// aDBXValue.AsDate := DateTimeToTimeStamp(aValue.AsExtended).Date;
// end
// else if CompareText(aFieldType, 'datetime') = 0 then
// begin
// aDBXValue.AsDateTime := FloatToDateTime(aValue.AsExtended);
// end
//
// else if CompareText(aFieldType, 'decimal') = 0 then
// begin
// aDBXValue.AsDouble := aValue.AsExtended;
// end
//
//
// else if CompareText(aFieldType, 'blob') = 0 then
// begin
// sourceStream := TStream(aValue.AsObject);
// if sourceStream = nil then
// aDBXValue.SetNull
// else
// begin
// str := TBytesStream.Create;
// try
// sourceStream.Position := 0;
// str.CopyFrom(sourceStream, sourceStream.Size);
// str.Position := 0;
// aDBXValue.SetStream(str, true);
// aDBXValue.ValueType.ValueTypeFlags :=
// aDBXValue.ValueType.ValueTypeFlags or TDBXValueTypeFlags.ExtendedType;
// except
// str.Free;
// raise;
// end;
// end;
// end
// else if CompareText(aFieldType, 'decimal') = 0 then
// begin
// aDBXValue.AsDouble := aValue.AsExtended;
// end
// else if CompareText(aFieldType, 'boolean') = 0 then
// begin
// if aValue.AsBoolean then
// aDBXValue.AsInt16 := 1
// else
// aDBXValue.AsInt16 := 0;
// end
// else
// raise Exception.Create('Unsupported type ' + IntToStr(ord(aValue.Kind)));
//
// end;

procedure TSqlite3PersistStrategy.SetLogger(ALogger: IdormLogger);
begin
  FLogger := ALogger;
end;

procedure TSqlite3PersistStrategy.StartTransaction;
begin
  if DB.InTransaction then
    raise Exception.Create('Transaction already active');
  DB.Start('trans');
end;

{ TSqlite3TableSequence }

function TSqlite3TableSequence.NewIntegerKey(const Entity: string): UInt64;
begin
  Result := FPersistStrategy.ExecuteAndGetFirst('SELECT GEN_ID(SEQ_' +
    Entity +
    '_ID, 1) FROM RDB$DATABASE');
end;

function TSqlite3TableSequence.NewStringKey(const Entity: string): string;
begin
  raise EdormException.Create('String keys not supported');
end;

class
  procedure TSqlite3TableSequence.RegisterClass;
begin
  // do nothing
end;

// procedure TSqlite3TableSequence.SetSqlite3Connection
// (const Value: TDBXFactory);
// begin
// FSqlite3Connection := Value;
// end;

procedure TSqlite3TableSequence.SetPersistStrategy(const PersistentStrategy
  : IdormPersistStrategy);
begin
  FPersistStrategy := PersistentStrategy;
end;

initialization

TSqlite3PersistStrategy.register;
TSqlite3TableSequence.RegisterClass;

finalization

end.
