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
  SQLiteTable3,
  dorm.adapter.DBExpress.Factory,
  Sqlite3,
  Rtti,
  dorm,
  superobject,
  TypInfo,
  FMTBcd,
  dorm.Collections;

type
  TSqlite3PersistStrategy = class(TdormInterfacedObject, IdormPersistStrategy)
  strict protected
    DB: TSQLiteDatabase;
    FLogger: IdormLogger;
    FKeysGeneratorClassName: string;
    FKeysGenerator: IdormKeysGenerator;
    FKeyType: TdormKeyType;
    FNullKeyValue: TValue;
    FLastInsertOID: TValue;
    // function CreateDBXFactory(Conf: ISuperObject): TDBXFactory; virtual;
    // function CreateObjectFromDBXReader(ARttiType: TRttiType;
    // AReader: TDBXReader; AFieldsMapping: TArray<TdormFieldMapping>): TObject;
    // function LoadObjectFromDBXReader(ARttiType: TRttiType; AReader: TDBXReader;
    // AFieldsMapping: TArray<TdormFieldMapping>; AObject: TObject): Boolean;
    function GetLogger: IdormLogger;
    procedure SetSqlite3ParameterValue(AQuery: TSQLitePreparedStatement;
      aFieldType: string;
      aParameterIndex: Integer; aValue: TValue);
  public
    // function GenerateAndFillPrimaryKeyParam(PKParam: TDBXParameter;
    // const Entity: string): TValue; overload;
    // function FillPrimaryKeyParam(PKParam: TDBXParameter;
    // const Value: TValue): TValue;
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
// var
// field: TdormFieldMapping;
// SQL: string;
// Query: TDBXCommand;
// I, pk_idx: Integer;
// v: TValue;
// sql_fields_names: string;
// pk_field: string;
begin
  // sql_fields_names := '';
  // for field in AFieldsMapping do
  // if not field.pk then
  // sql_fields_names := sql_fields_names + ',"' + field.field + '" = ?';
  // System.Delete(sql_fields_names, 1, 1);
  //
  // pk_field := AFieldsMapping[GetPKMappingIndex(AFieldsMapping)].field;
  // SQL := Format('UPDATE %S SET %S WHERE %S = ?', [ATableName, sql_fields_names,
  // pk_field]);
  //
  // GetLogger.Debug(AFieldsMapping[GetPKMappingIndex(AFieldsMapping)].field);
  //
  // GetLogger.Debug('PREPARING: ' + SQL);
  // Query := FB.Prepare(SQL);
  // try
  // I := 0;
  // for field in AFieldsMapping do
  // begin
  // v := TdormUtils.GetField(AObject, field.name);
  //
  // if field.pk then
  // Continue
  // else
  // begin
  // SetDBXParameterValue(field.field_type, Query.Parameters[I], v);
  // end;
  // inc(I);
  // end;
  // pk_idx := GetPKMappingIndex(AFieldsMapping);
  //
  // v := ARttiType.GetProperty(AFieldsMapping[pk_idx].name).GetValue(AObject);
  // FillPrimaryKeyParam(Query.Parameters[I], v);
  // GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
  // FB.Execute(Query);
  // finally
  // Query.Free;
  // end;
end;

procedure TSqlite3PersistStrategy.Commit;
begin
  if not DB.IsTransactionOpen then
    raise Exception.Create('Transaction is not active');
  DB.Commit;
end;

procedure TSqlite3PersistStrategy.ConfigureStrategy(ConfigurationInfo
  : ISuperObject);
var
  ctx: TRttiContext;
  t: TRttiType;
  obj: TObject;
  database_connection_string: string;
  password: string;
begin
  database_connection_string := ConfigurationInfo.S
    ['database_connection_string'];
  password := ConfigurationInfo.S['password'];
  DB := TSQLiteDatabase.Create(database_connection_string, seUTF8, password);
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
// var
// cmd: TDBXCommand;
// reader: TDBXReader;
// SQL: string;
begin
  // Result := -1;
  // SQL := 'SELECT COUNT(*) FROM ' + ATableName;
  // GetLogger.Debug('PREPARING: ' + SQL);
  // cmd := FB.Prepare(SQL);
  // try
  // reader := cmd.ExecuteQuery;
  // try
  // if reader.Next then
  // Result := reader.Value[0].AsInt64;
  // finally
  // reader.Free;
  // end;
  // finally
  // cmd.Free;
  // end;
end;

// function TSqlite3PersistStrategy.CreateChildLoaderSearch(AChildClassTypeInfo
// : PTypeInfo; AChildClassName, AChildTableName, AChildRelationField: string;
// APKValue: TValue): IdormSearchCriteria;
// var
// SQL: string;
// begin
// SQL := 'SELECT * FROM ' + AChildTableName + ' WHERE ' +
// AChildRelationField + ' = ';
// case FKeyType of
// ktInteger:
// SQL := SQL + IntToStr(APKValue.AsInt64);
// ktString:
// SQL := SQL + '''' + StringReplace(APKValue.AsString, '''', '''''',
// [rfReplaceAll]) + '''';
// end;
//
// GetLogger.Debug('CreateChildLoaderSearch: ' + SQL);
// Result := TdormSimpleSearchCriteria.Create(nil, SQL);
// end;

// function TSqlite3PersistStrategy.CreateDBXFactory(Conf: ISuperObject)
// : TDBXFactory;
// begin
// Result := TDBXFactory.Create('dbxfb.dll', 'Sqlite3', Conf);
// end;

function TSqlite3PersistStrategy.Delete(ARttiType: TRttiType;
  AObject: TObject;
  ATableName: string; AFieldsMapping: TArray<TdormFieldMapping>): TObject;
// var
// pk_idx: Integer;
// pk_value: TValue;
// pk_attribute_name, pk_field_name, SQL: string;
// cmd: TDBXCommand;
begin
  // pk_idx := GetPKMappingIndex(AFieldsMapping);
  // if pk_idx = -1 then
  // raise Exception.Create('Invalid primary key for table ' + ATableName);
  // pk_attribute_name := AFieldsMapping[pk_idx].name;
  // pk_field_name := AFieldsMapping[pk_idx].field;
  // pk_value := ARttiType.GetProperty(pk_attribute_name).GetValue(AObject);
  // SQL := 'DELETE FROM ' + ATableName + ' WHERE ' + pk_field_name + ' = ?';
  // GetLogger.Debug('PREPARING: ' + SQL);
  // cmd := FB.Prepare(SQL);
  // try
  // FillPrimaryKeyParam(cmd.Parameters[pk_idx], pk_value);
  // GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
  // cmd.ExecuteUpdate;
  // finally
  // cmd.Free;
  // end;
  //
end;

procedure TSqlite3PersistStrategy.DeleteAll(ATableName: string);
var
  SQL: string;
begin
  SQL := 'DELETE FROM ' + ATableName;
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
// var
// rdr: TDBXReader;
// cmd: TDBXCommand;
begin
  // cmd := FB.Prepare(SQL);
  // try
  // rdr := cmd.ExecuteQuery;
  // try
  // if rdr.Next then
  // Result := rdr.Value[0].AsInt64
  // else
  // raise EdormException.Create('ExecuteAndGetFirst returns o rows');
  // rdr.Close;
  // finally
  // rdr.Free;
  // end;
  // finally
  // cmd.Free;
  // end;
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

// function TSqlite3PersistStrategy.FillPrimaryKeyParam(PKParam: TDBXParameter;
// const Value: TValue): TValue;
// begin
// try
// case FKeyType of
// ktString:
// begin
// PKParam.Value.SetString(Value.AsString);
// Result := PKParam.Value.GetString;
// end;
// ktInteger:
// begin
// PKParam.Value.AsInt64 := Value.AsInt64;
// Result := PKParam.Value.AsInt64;
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
  Result := FKeysGenerator;
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
  Query: TSQLitePreparedStatement;
  Param, I, pk_idx: Integer;
  v, pk_value: TValue;
begin
  sql_fields_names := '';
  for field in AFieldsMapping do
    if not field.pk then
      if field.field_type <> 'blob' then
        sql_fields_names := sql_fields_names + ',' + field.field + '';

  System.Delete(sql_fields_names, 1, 1);
  Param := 0;
  sql_fields_values := '';
  for field in AFieldsMapping do
    if not field.pk then
    begin
      if field.field_type <> 'blob' then
      begin
        sql_fields_values := sql_fields_values + ',?';
        inc(Param);
      end;
    end;
  System.Delete(sql_fields_values, 1, 1);

  SQL := Format('INSERT INTO %s (%S) VALUES (%S)',
    [ATableName, sql_fields_names, sql_fields_values]);
  GetLogger.Debug('PREPARING :' + SQL);

  Query := TSQLitePreparedStatement.Create(DB, SQL, []);
  try
    // Query.PrepareStatement(SQL);
    CodeSite.Send(inttostr(Query.ParamCount));
    I := 1;
    for field in AFieldsMapping do
    begin
      v := TdormUtils.GetField(AObject, field.name);
      if field.pk then
      begin
        // pk_value := GenerateAndFillPrimaryKeyParam(Query.Parameters[I],
        // ATableName)
      end
      else
      begin
        if field.field_type <> 'blob' then
        begin
          SetSqlite3ParameterValue(Query, field.field_type, I, v);
          inc(I);
        end;
      end;
    end;
    GetLogger.Debug('EXECUTING PREPARED :' + SQL);
    if not Query.ExecSQL then
      raise EdormException.Create('Error Message');
  finally
    Query.Free;
  end;
  pk_idx := GetPKMappingIndex(AFieldsMapping);
  TdormUtils.SetProperty(AObject, AFieldsMapping[pk_idx].name, pk_value);
  Result := pk_value;
end;

function TSqlite3PersistStrategy.InTransaction: Boolean;
begin
  // Result := assigned(FCurTransaction);
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
// var
// SQL: string;
// cmd: TDBXCommand;
// reader: TDBXReader;
begin
  // SQL := AdormSearchCriteria.GetSQL;
  // GetLogger.Debug('PREPARING: ' + SQL);
  // cmd := FB.Prepare(SQL);
  // try
  // GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
  // reader := cmd.ExecuteQuery;
  // try
  // while reader.Next do
  // TdormUtils.MethodCall(AList, 'Add',
  // [CreateObjectFromDBXReader(ARttiType, reader,
  // AFieldsMapping)]);
  // // AList.Add(CreateObjectFromDBXReader(ARttiType, reader,
  // // AFieldsMapping));
  // finally
  // reader.Free;
  // end;
  // finally
  // cmd.Free;
  // end;
end;

function TSqlite3PersistStrategy.Load(ARttiType: TRttiType;
  ATableName: string;
  AFieldsMapping: TArray<TdormFieldMapping>; const Value: TValue): TObject;
// var
// pk_idx: Integer;
// pk_attribute_name, pk_field_name, SQL: string;
// cmd: TDBXCommand;
// reader: TDBXReader;
begin
  // Result := nil;
  // pk_idx := GetPKMappingIndex(AFieldsMapping);
  // if pk_idx = -1 then
  // raise Exception.Create('Invalid primary key for table ' + ATableName);
  // pk_attribute_name := AFieldsMapping[pk_idx].name;
  // pk_field_name := AFieldsMapping[pk_idx].field;
  // SQL := 'SELECT ' + GetSelectFieldsList(AFieldsMapping, true) + ' FROM ' +
  // ATableName + ' WHERE ' + pk_field_name + ' = ?';
  // GetLogger.Debug('PREPARING: ' + SQL);
  // cmd := FB.Prepare(SQL);
  // try
  // FillPrimaryKeyParam(cmd.Parameters[pk_idx], Value);
  // GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
  // reader := cmd.ExecuteQuery();
  // try
  // if reader.Next then
  // Result := CreateObjectFromDBXReader(ARttiType, reader, AFieldsMapping);
  // finally
  // reader.Free;
  // end;
  // finally
  // cmd.Free;
  // end;
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
  // Result :=
  DB.ExecSQL(SQL); // sqlite3 do not return affected rows?
end;

// function TSqlite3PersistStrategy.CreateObjectFromDBXReader
// (ARttiType: TRttiType; AReader: TDBXReader;
// AFieldsMapping: TArray<TdormFieldMapping>): TObject;
// var
// obj: TObject;
// field: TdormFieldMapping;
// v: TValue;
// t: TTimeStamp;
// S: string;
// sourceStream: TStream;
// targetStream: TMemoryStream;
// v1: TValue;
// begin
// Result := nil;
// try
// obj := TdormUtils.CreateObject(ARttiType);
// for field in AFieldsMapping do
// begin
// if CompareText(field.field_type, 'string') = 0 then
// begin
// v := AReader.Value[AReader.GetOrdinal(field.field)].AsString;
// S := field.field + ' as string';
// end
// else if CompareText(field.field_type, 'integer') = 0 then
// begin
// v := AReader.Value[AReader.GetOrdinal(field.field)].AsInt32;
// S := field.field + ' as integer';
// end
// else if CompareText(field.field_type, 'date') = 0 then
// begin
// t.Date := AReader.Value[AReader.GetOrdinal(field.field)].AsDate;
// t.Time := 0;
// v := TimeStampToDateTime(t);
// S := field.field + ' as date';
// end
// else if CompareText(field.field_type, 'blob') = 0 then
// begin
// targetStream := nil;
// sourceStream := AReader.Value[AReader.GetOrdinal(field.field)].AsStream;
// S := field.field + ' as blob';
// if assigned(sourceStream) then
// begin
// sourceStream.Position := 0;
// targetStream := TMemoryStream.Create;
// targetStream.CopyFrom(sourceStream, sourceStream.Size);
// targetStream.Position := 0;
// end;
// v := targetStream;
// end
// else if CompareText(field.field_type, 'decimal') = 0 then
// begin
// v := AReader.Value[AReader.GetOrdinal(field.field)].AsDouble;
// S := field.field + ' as decimal';
// end
// else if CompareText(field.field_type, 'boolean') = 0 then
// begin
// v := AReader.Value[AReader.GetOrdinal(field.field)].AsBoolean;
// S := field.field + ' as boolean';
// end
// else if CompareText(field.field_type, 'datetime') = 0 then
// begin
// v := AReader.Value[AReader.GetOrdinal(field.field)].AsDateTime;
// S := field.field + ' as datetime';
// end
// else
// raise Exception.Create('Unknown field type for ' + field.field);
// try
// TdormUtils.SetField(obj, field.name, v);
// except
// on E: Exception do
// begin
//
// raise EdormException.Create(E.Message + sLineBreak +
// '. Probably cannot write ' + ARttiType.ToString + '.' + S);
// end;
// end;
// end;
// except
// on E: Exception do
// begin
//
// raise;
// end;
// end;
// Result := obj;
// end;

class
  procedure TSqlite3PersistStrategy.register;
begin
  //
end;

procedure TSqlite3PersistStrategy.Rollback;
begin
  if not DB.IsTransactionOpen then
    raise Exception.Create('Transaction is not active');
  DB.Rollback;
end;

procedure TSqlite3PersistStrategy.SetSqlite3ParameterValue
  (AQuery: TSQLitePreparedStatement;
  aFieldType: string; aParameterIndex: Integer; aValue: TValue);
var
  sourceStream: TStream;
  str: TBytesStream;
begin
  if CompareText(aFieldType, 'string') = 0 then
  begin
    AQuery.SetParamText(aParameterIndex, aValue.AsString);
  end
  else if CompareText(aFieldType, 'decimal') = 0 then
  begin
    AQuery.SetParamFloat(aParameterIndex, aValue.AsExtended);
  end
  else if CompareText(aFieldType, 'boolean') = 0 then
  begin
    if aValue.AsBoolean then
      AQuery.SetParamInt(aParameterIndex, 1)
    else
      AQuery.SetParamInt(aParameterIndex, 0);
  end
  else if CompareText(aFieldType, 'date') = 0 then
  begin
    AQuery.SetParamDate(aParameterIndex,
      DateTimeToTimeStamp(aValue.AsExtended).Date);
  end
  else if CompareText(aFieldType, 'datetime') = 0 then
  begin
    AQuery.SetParamDate(aParameterIndex,
      FloatToDateTime(aValue.AsExtended));
  end
  else if CompareText(aFieldType, 'blob') = 0 then
  begin
    {
      sourceStream := TStream(aValue.AsObject);
      if sourceStream = nil then
      begin
      // AQuery.SetParamBlob(aParameterIndex, nil)
      end
      else
      begin
      str := TBytesStream.Create;
      try
      sourceStream.Position := 0;
      str.CopyFrom(sourceStream, sourceStream.Size);
      str.Position := 0;
      AQuery.SetParamBlob(aParameterIndex, str);
      except
      str.Free;
      raise;
      end;
      end;
    }
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
  if DB.IsTransactionOpen then
    raise Exception.Create('Transaction already active');
  DB.BeginTransaction;
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
