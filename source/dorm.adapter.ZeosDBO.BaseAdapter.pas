unit dorm.adapter.ZeosDBO.BaseAdapter;

interface

uses
  classes,
  SysUtils,
  TypInfo,
  DB,
  Rtti,
  Generics.Collections,
  superobject,
  FMTBcd,
  dorm,
  dorm.Commons,
  dorm.Mappings,
  dorm.Filters,
  dorm.Collections,
  dorm.adapter.ZeosDBO.Facade,
  dorm.adapter.Base,
  dorm.Mappings.Strategies,
  ZAbstractRODataset,
  ZDataset
  ;



type
  TZeosDBOBaseAdapter = class(TBaseAdapter, IdormPersistStrategy)
  strict private
    function Load(ARttiType: TRttiType; ATableName: string; AMappingTable: TMappingTable;
      const Value: TValue): TObject; overload;

  private
    function GetZeosDBOReaderFor(ARttiType: TRttiType; AMappingTable: TMappingTable;
      const Value: TValue; AMappingRelationField: TMappingField = nil): TZQuery;

  protected
    FFormatSettings: TFormatSettings;
    FB: TZeosDBOFacade;
    FLogger: IdormLogger;
    FKeysGeneratorClassName: string;
    FKeysGenerator: IdormKeysGenerator;
    FKeyType: TdormKeyType;
    FNullKeyValue: TValue;
    FLastInsertOID: TValue;
    procedure InitFormatSettings;
    function CreateZeosDBOFacade(Conf: ISuperObject): TZeosDBOFacade; virtual; abstract;
    function CreateObjectFromZeosDBOQuery(ARttiType: TRttiType; AReader: TZQuery;
      AMappingTable: TMappingTable): TObject;
    procedure LoadObjectFromZeosDBOReader(AObject: TObject; ARttiType: TRttiType; AReader: TZQuery;
      AFieldsMapping: TMappingFieldList);
    function GetLogger: IdormLogger;
    procedure SetZeosDBOParameterValue(AFieldType: string; AStatement: TZQuery;
      ParameterIndex: Integer; AValue: TValue; IsNullable: boolean);
    function CanBeConsideredAsNull(const AValue: TValue): boolean;
  public
    class procedure register;

    destructor Destroy; override;

    function GenerateAndFillPrimaryKeyParam(Query: TZQuery; ParamIndex: Integer;
      const Entity: string): TValue; overload;
    function FillPrimaryKeyParam(Query: TZQuery; ParamIndex: Integer;
      const Value: TValue): TValue;
    function GetLastInsertOID: TValue;
    function GetKeysGenerator: IdormKeysGenerator;
    function Insert(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable): TValue;
    function Update(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable;
      ACurrentVersion: Int64): Int64;
    function Delete(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable;
      ACurrentVersion: Int64): Int64;
    procedure DeleteAll(AMappingTable: TMappingTable);
    function Count(AMappingTable: TMappingTable): Int64;
    function Load(ARttiType: TRttiType; AMappingTable: TMappingTable;
      AMappingRelationField: TMappingField; const Value: TValue; AObject: TObject)
      : boolean; overload;
    function Load(ARttiType: TRttiType; AMappingTable: TMappingTable; const Value: TValue;
      AObject: TObject): boolean; overload;
    function List(ARttiType: TRttiType; AMappingTable: TMappingTable; ACriteria: ICriteria)
      : TObjectList<TObject>;
    procedure LoadList(AList: TObject; ARttiType: TRttiType; AMappingTable: TMappingTable;
      ACriteria: ICriteria); overload;
    procedure ConfigureStrategy(ConfigurationInfo: ISuperObject); virtual;
    procedure InitStrategy;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function InTransaction: boolean;
    procedure SetLogger(ALogger: IdormLogger);
    function IsNullKey(const Value: TValue): boolean;
    function GetNullKeyValue: TValue;
    function GetKeyType: TdormKeyType;
    function RawExecute(SQL: string): Int64;
    function ExecuteAndGetFirst(SQL: string): Int64;
    function GetDatabaseBuilder(AEntities: TList<string>; AMappings: ICacheMappingStrategy)
      : IDataBaseBuilder;
    function ExecuteCommand(ACommand: IdormCommand): Int64;
  end;

  TZeosDBOBaseTableSequence = class abstract(TdormInterfacedObject, IdormKeysGenerator)
  protected
    FPersistStrategy: IdormPersistStrategy;
    function GetSequenceFormatTemplate: string; virtual; abstract;
  public
    function NewStringKey(const Entity: string): string;
    function NewIntegerKey(const Entity: string): UInt64;
    procedure SetPersistStrategy(const PersistentStrategy: IdormPersistStrategy);
  end;


implementation

uses
  dorm.Utils;



{ TZeosDBOBaseAdapter }

function TZeosDBOBaseAdapter.CanBeConsideredAsNull(
  const AValue: TValue): boolean;
var
  v: Extended;
  s: String;
begin
  case AValue.Kind of
    tkInteger:
      begin
        Result := AValue.AsInteger = Self.FNullKeyValue.AsInteger;
      end;
    tkInt64:
      begin
        Result := AValue.AsInt64 = Self.FNullKeyValue.AsInteger;
      end;
    tkFloat:
      begin
        v := AValue.AsExtended;
        Result := (Frac(v) = 0) and (Trunc(v) = Self.FNullKeyValue.AsInteger);
      end;
    tkString, tkUString, tkWString, tkLString, tkWChar, tkChar:
      begin
        Result := AValue.AsString = '';
      end
  else
    Result := false;
  end;
end;

procedure TZeosDBOBaseAdapter.Commit;
begin
  FB.CommitTransaction;
end;

procedure TZeosDBOBaseAdapter.ConfigureStrategy(
  ConfigurationInfo: ISuperObject);
var
  ctx: TRttiContext;
  t: TRttiType;
  obj: TObject;
begin
  FB := CreateZeosDBOFacade(ConfigurationInfo);
  FKeysGeneratorClassName := ConfigurationInfo.s['keys_generator'];
  t := ctx.FindType(FKeysGeneratorClassName);

  if t = nil then
    raise EdormException.Create('Unknown key generator ' + FKeysGeneratorClassName);

  obj := t.AsInstance.MetaclassType.Create;

  if not Supports(obj, IdormKeysGenerator, FKeysGenerator) then
    raise EdormException.Create('Keys generator ' + FKeysGeneratorClassName +
      ' doesn''t implements ''IdormKeysGenerator''');

  FKeysGenerator.SetPersistStrategy(Self);
  Self._Release;

  if (SameText(ConfigurationInfo.s['key_type'], 'integer')) then
  begin
    FKeyType := ktInteger;
    FNullKeyValue := ConfigurationInfo.I['null_key_value']
  end
  else if (SameText(ConfigurationInfo.s['key_type'], 'string')) then
  begin
    FKeyType := ktString;
    FNullKeyValue := ConfigurationInfo.s['null_key_value']
  end
  else
    raise EdormException.Create('Unknown key type');

  inherited;
end;

function TZeosDBOBaseAdapter.Count(AMappingTable: TMappingTable): Int64;
var
  cmd: TZQuery;
  SQL: string;
begin
  Result := -1;
  SQL := 'SELECT COUNT(*) FROM ' + AMappingTable.TableName;
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);

  try
    cmd.Open;
    if not cmd.Eof then
      Result := cmd.Fields[0].AsInteger;
  finally
    cmd.Free;
  end;
end;

function TZeosDBOBaseAdapter.CreateObjectFromZeosDBOQuery(ARttiType: TRttiType;
  AReader: TZQuery; AMappingTable: TMappingTable): TObject;
var
  obj: TObject;
  field: TMappingField;
  v: TValue;
  s: string;
  targetStream: TMemoryStream;
begin
  try
    obj := TdormUtils.CreateObject(ARttiType);

    for field in AMappingTable.Fields do
    begin
      if CompareText(field.FieldType, 'string') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsString;
        s := field.FieldName + ' as string';
      end
      else if CompareText(field.FieldType, 'integer') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsInteger;
        s := field.FieldName + ' as integer';
      end
      else if CompareText(field.FieldType, 'date') = 0 then
      begin
        v := Trunc(AReader.FieldByName(field.FieldName).AsDateTime);
        s := field.FieldName + ' as date';
      end
      else if CompareText(field.FieldType, 'blob') = 0 then
      begin
        targetStream := nil;

        if not AReader.FieldByName(field.FieldName).IsNull then
        begin
          targetStream := TMemoryStream.Create;
          targetStream.Size := AReader.FieldByName(field.FieldName).DataSize;

          AReader.FieldByName(field.FieldName).GetData(targetStream.Memory);
          targetStream.Position := 0;
        end;

        s := field.FieldName + ' as blob';
        v := targetStream;
      end
      else if CompareText(field.FieldType, 'decimal') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsFloat;
        s := field.FieldName + ' as decimal';
      end
      else if CompareText(field.FieldType, 'boolean') = 0 then
      begin
        if AReader.FieldByName(field.FieldName).DataType = ftBoolean then
          v := AReader.FieldByName(field.FieldName).AsBoolean
        else
          v := AReader.FieldByName(field.FieldName).AsInteger = 1;

        s := field.FieldName + ' as boolean';
      end
      else if CompareText(field.FieldType, 'datetime') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        s := field.FieldName + ' as datetime';
      end
      else if CompareText(field.FieldType, 'time') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        s := field.FieldName + ' as datetime';
      end
      else
        raise Exception.Create('Unknown field type for ' + field.FieldName);

      try
        TdormUtils.SetField(obj, field.name, v);
      except
        on E: Exception do
        begin
          raise EdormException.Create(E.Message + sLineBreak + '. Probably cannot write ' +
            ARttiType.ToString + '.' + s);
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

function TZeosDBOBaseAdapter.Delete(ARttiType: TRttiType; AObject: TObject;
  AMappingTable: TMappingTable; ACurrentVersion: Int64): Int64;
var
  pk_idx: Integer;
  pk_value: TValue;
  pk_attribute_name, pk_field_name, SQL: string;
  cmd: TZQuery;
begin
  pk_idx := GetPKMappingIndex(AMappingTable.Fields);

  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + AMappingTable.TableName);

  pk_attribute_name := AMappingTable.Fields[pk_idx].name;
  pk_field_name := AMappingTable.Fields[pk_idx].FieldName;
  pk_value := ARttiType.GetProperty(pk_attribute_name).GetValue(AObject);
  SQL := 'DELETE FROM ' + AMappingTable.TableName + ' WHERE ' + pk_field_name + ' = :' + pk_field_name;

  if ACurrentVersion >= 0 then
    SQL := SQL + ' AND OBJVERSION = ' + IntToStr(ACurrentVersion);
  // else
  // raise EdormLockingException.Create('Invalid ObjVersion');
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);

  try
    FillPrimaryKeyParam(cmd, 0, pk_value);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    cmd.ExecSQL;

    Result := cmd.RowsAffected;
  finally
    cmd.Free;
  end;
end;

procedure TZeosDBOBaseAdapter.DeleteAll(AMappingTable: TMappingTable);
var
  SQL: string;
begin
  SQL := 'DELETE FROM ' + AMappingTable.TableName;
  GetLogger.Debug('EXECUTING :' + SQL);
  FB.Execute(SQL);
end;

destructor TZeosDBOBaseAdapter.Destroy;
begin
  FB.Free;
  FKeysGenerator := nil;
  inherited;
end;

function TZeosDBOBaseAdapter.ExecuteAndGetFirst(SQL: string): Int64;
var
  cmd: TZQuery;
begin
  GetLogger.EnterLevel('ExecuteAndGetFirst');
  Result := 0;
  GetLogger.Info('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);

  try
    GetLogger.Info('EXECUTING: ' + SQL);
    cmd.Open;

    if not cmd.Eof then
      Result := Int64(cmd.Fields[0].AsInteger)
    else
      raise EdormException.Create('ExecuteAndGetFirst returns o rows');
  finally
    GetLogger.ExitLevel('ExecuteAndGetFirst');
    cmd.Free;
  end;
end;

function TZeosDBOBaseAdapter.ExecuteCommand(ACommand: IdormCommand): Int64;
var
  SQL: string;
  reader: TZQuery;
  CustomCriteria: ICustomCriteria;
begin
  SQL := ACommand.GetSQL;
  GetLogger.Debug('EXECUTING: ' + SQL);
  reader := FB.Prepare(SQL);
  try
    if reader.Params.Count <> 0 then
      raise EdormException.Create('Parameters not replaced');
    reader.ExecSQL;

    Result := reader.RowsAffected;
  finally
    reader.Free;
  end;
end;

function TZeosDBOBaseAdapter.FillPrimaryKeyParam(Query: TZQuery;
  ParamIndex: Integer; const Value: TValue): TValue;
begin
  try
    case FKeyType of
      ktString:
        begin
          Query.Params[ParamIndex].AsString := Value.AsString;
          Result := Query.Params[ParamIndex].AsString;
          GetLogger.Debug('ParPK = ' + Value.AsString);
        end;
      ktInteger:
        begin
          Query.Params[ParamIndex].AsInteger := Value.AsInteger;
          Result := Query.Params[ParamIndex].AsInteger;
          GetLogger.Debug('ParPK = ' + IntToStr(Value.AsInteger));
        end;
    end;
  except
    on E: Exception do
      raise EdormException.Create('Error during fill primary key for query. ' + E.Message);
  end;
end;

function TZeosDBOBaseAdapter.GenerateAndFillPrimaryKeyParam(
  Query: TZQuery; ParamIndex: Integer; const Entity: string): TValue;
var
  Value: TValue;
begin
  case FKeyType of
    ktString:
      begin
        Value := FKeysGenerator.NewStringKey(Entity);
      end;
    ktInteger:
      begin
        Value := Int64(FKeysGenerator.NewIntegerKey(Entity));
      end;
  end;
  // Assert(Value.IsType<Int64>());
  Result := FillPrimaryKeyParam(Query, ParamIndex, Value);
  FLastInsertOID := Result;
end;

function TZeosDBOBaseAdapter.GetDatabaseBuilder(AEntities: TList<string>;
  AMappings: ICacheMappingStrategy): IDataBaseBuilder;
begin
  AEntities.Free; // just to hide the memory leak
  raise Exception.Create('Not implemented for ' + Self.ClassName);
end;

function TZeosDBOBaseAdapter.GetKeysGenerator: IdormKeysGenerator;
begin
  Result := FKeysGenerator;
end;

function TZeosDBOBaseAdapter.GetKeyType: TdormKeyType;
begin
  Result := FKeyType;
end;

function TZeosDBOBaseAdapter.GetLastInsertOID: TValue;
begin
  Result := FLastInsertOID;
end;

function TZeosDBOBaseAdapter.GetLogger: IdormLogger;
begin
  Result := FLogger;
end;

function TZeosDBOBaseAdapter.GetNullKeyValue: TValue;
begin
  Result := FNullKeyValue;
end;

function TZeosDBOBaseAdapter.GetZeosDBOReaderFor(ARttiType: TRttiType;
  AMappingTable: TMappingTable; const Value: TValue;
  AMappingRelationField: TMappingField): TZQuery;
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
    SQL := 'SELECT ' + GetSelectFieldsList(AMappingTable.Fields, true) + ' FROM ' +
      AMappingTable.TableName + ' WHERE ' + pk_field_name + ' = :' + pk_field_name;
  end
  else
  begin
    pk_idx := GetPKMappingIndex(AMappingTable.Fields);

    if pk_idx = -1 then
      raise Exception.Create('Invalid primary key for table ' + AMappingTable.TableName);

    pk_field_name := AMappingTable.Fields[pk_idx].FieldName;

    SQL := 'SELECT ' + GetSelectFieldsList(AMappingTable.Fields, true) + ' FROM ' +
      AMappingTable.TableName + ' WHERE ' + AMappingRelationField.FieldName + ' = :' +
      pk_field_name;
  end;

  GetLogger.Debug('PREPARING: ' + SQL);
  Result := FB.Prepare(SQL);
  FillPrimaryKeyParam(Result, 0, Value);
end;

procedure TZeosDBOBaseAdapter.InitFormatSettings;
begin
  FFormatSettings.LongDateFormat := 'YYYY-MM-DD';
  FFormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  FFormatSettings.LongTimeFormat := 'HH:NN:SS';
  FFormatSettings.ShortTimeFormat := 'HH:NN:SS';
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.TimeSeparator := ':';
end;

procedure TZeosDBOBaseAdapter.InitStrategy;
begin
  FLastInsertOID := TValue.Empty;
end;

function TZeosDBOBaseAdapter.Insert(ARttiType: TRttiType; AObject: TObject;
  AMappingTable: TMappingTable): TValue;
var
  field: TMappingField;
  sql_fields_names, sql_fields_values, SQL: ansistring;
  Query: TZQuery;
  I, pk_idx: Integer;
  v, pk_value: TValue;
begin
  sql_fields_names := '';

  for field in AMappingTable.Fields do
  begin { todo: manage transients fields }
    sql_fields_names := sql_fields_names + ',"' + ansistring(field.FieldName) + '"';
  end;

  System.Delete(sql_fields_names, 1, 1);
  sql_fields_values := '';

  for I := 0 to AMappingTable.Fields.Count - 1 do
    // for field in AMappingTable.Fields do
    sql_fields_values := sql_fields_values + ',:' + AMappingTable.Fields[I].Name;

  System.Delete(sql_fields_values, 1, 1);

  SQL := Format('INSERT INTO %s (%S) VALUES (%S)', [AMappingTable.TableName, sql_fields_names,
    sql_fields_values]);

  GetLogger.Debug('PREPARING :' + string(SQL));
  Query := FB.Prepare(string(SQL));

  try
    I := 0;

    for field in AMappingTable.Fields do
    begin
      // v := TdormUtils.GetField(AObject, field.name);
      v := TdormUtils.GetField(AObject, field.RTTICache);

      if field.IsPK then
        pk_value := GenerateAndFillPrimaryKeyParam(Query, I, AMappingTable.TableName)
      else
      begin
        SetZeosDBOParameterValue(field.FieldType, Query, I, v, field.Nullable);
      end;

      inc(I);
    end;

    GetLogger.Debug('EXECUTING PREPARED :' + string(SQL));
    FB.Execute(Query);
  finally
    Query.Free;
  end;

  pk_idx := GetPKMappingIndex(AMappingTable.Fields);
  // TdormUtils.SetField(AObject, AMappingTable.Fields[pk_idx].name, pk_value);
  TdormUtils.SetField(AObject, AMappingTable.Fields[pk_idx].RTTICache, pk_value);
  Result := pk_value;
end;

function TZeosDBOBaseAdapter.InTransaction: boolean;
begin
  Result := FB.GetConnection.InTransaction;
end;

function TZeosDBOBaseAdapter.IsNullKey(const Value: TValue): boolean;
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

function TZeosDBOBaseAdapter.List(ARttiType: TRttiType;
  AMappingTable: TMappingTable; ACriteria: ICriteria): TObjectList<TObject>;
begin
  Result := NewList();
  LoadList(Result, ARttiType, AMappingTable, ACriteria);
end;

function TZeosDBOBaseAdapter.Load(ARttiType: TRttiType;
  AMappingTable: TMappingTable; const Value: TValue; AObject: TObject): boolean;
var
  reader: TZQuery;
begin
  reader := GetZeosDBOReaderFor(ARttiType, AMappingTable, Value);

  try
    reader.Open();
    Result := not reader.Eof;

    if Result then
      LoadObjectFromZeosDBOReader(AObject, ARttiType, reader, AMappingTable.Fields);
  finally
    reader.Free;
  end;
end;

function TZeosDBOBaseAdapter.Load(ARttiType: TRttiType; ATableName: string;
  AMappingTable: TMappingTable; const Value: TValue): TObject;
var
  pk_idx: Integer;
  pk_attribute_name, pk_field_name, SQL: string;
  cmd: TZQuery;
begin
  Result := nil;
  pk_idx := GetPKMappingIndex(AMappingTable.Fields);

  if pk_idx = -1 then
    raise Exception.Create('Invalid primary key for table ' + ATableName);

  pk_attribute_name := AMappingTable.Fields[pk_idx].name;
  pk_field_name := AMappingTable.Fields[pk_idx].FieldName;

  SQL := 'SELECT ' + GetSelectFieldsList(AMappingTable.Fields, true) + ' FROM ' + ATableName +
    ' WHERE ' + pk_field_name + ' = :' + pk_field_name;

  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FB.Prepare(SQL);

  try
    FillPrimaryKeyParam(cmd, pk_idx, Value);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    cmd.Open;

    if not cmd.Eof then
      Result := CreateObjectFromZeosDBOQuery(ARttiType, cmd, AMappingTable);
  finally
    cmd.Free;
  end;
end;

function TZeosDBOBaseAdapter.Load(ARttiType: TRttiType;
  AMappingTable: TMappingTable; AMappingRelationField: TMappingField;
  const Value: TValue; AObject: TObject): boolean;
var
  reader: TZQuery;
begin
  reader := GetZeosDBOReaderFor(ARttiType, AMappingTable, Value, AMappingRelationField);
  try
    reader.Open();
    Result := not reader.Eof;

    if Result then
      LoadObjectFromZeosDBOReader(AObject, ARttiType, reader, AMappingTable.Fields);

    reader.Next;

    if not reader.Eof then
      // there is some problem.... here I should have only one record
      raise EdormException.Create('Singleton select returns more than 1 record');
  finally
    reader.Free;
  end;
end;

procedure TZeosDBOBaseAdapter.LoadList(AList: TObject; ARttiType: TRttiType;
  AMappingTable: TMappingTable; ACriteria: ICriteria);
var
  SQL: string;
  reader: TZQuery;
  CustomCriteria: ICustomCriteria;
begin
  if assigned(ACriteria) and TInterfacedObject(ACriteria).GetInterface(ICustomCriteria,
    CustomCriteria) then
    SQL := CustomCriteria.GetSQL
  else
    SQL := Self.GetSelectSQL(ACriteria, AMappingTable);

  GetLogger.Debug('EXECUTING: ' + SQL);
  reader := FB.Prepare(SQL);

  if reader.Params.Count <> 0 then
    raise EdormException.Create('Parameters not replaced');

  reader.Open();

  try
    while not reader.Eof do
    begin
      TdormUtils.MethodCall(AList, 'Add', [CreateObjectFromZeosDBOQuery(ARttiType, reader,
        AMappingTable)]);
      reader.Next;
    end;
  finally
    reader.Free;
  end;
end;

procedure TZeosDBOBaseAdapter.LoadObjectFromZeosDBOReader(AObject: TObject;
  ARttiType: TRttiType; AReader: TZQuery; AFieldsMapping: TMappingFieldList);
var
  field: TMappingField;
  v: TValue;
  s: string;
  sourceStream: TStream;
begin
  try
    for field in AFieldsMapping do
    begin
      if CompareText(field.FieldType, 'string') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsString;
        s := field.FieldName + ' as string';
      end
      else if CompareText(field.FieldType, 'integer') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsInteger;
        s := field.FieldName + ' as integer';
      end
      else if CompareText(field.FieldType, 'date') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        s := field.FieldName + ' as date';
      end
      else if CompareText(field.FieldType, 'blob') = 0 then
      begin
        // targetStream := nil;
        s := field.FieldName + ' as blob';
        sourceStream := nil;
        if not AReader.FieldByName(field.FieldName).IsNull then
        begin
          sourceStream := TMemoryStream.Create;
//          AReader.ReadBlob(AReader.Fields.GetFieldIndex(field.FieldName), sourceStream);
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
        s := field.FieldName + ' as decimal';
      end
      else if CompareText(field.FieldType, 'boolean') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsInteger;
        v := v.AsInteger = 1;
        s := field.FieldName + ' as boolean';
      end
      else if CompareText(field.FieldType, 'datetime') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        s := field.FieldName + ' as datetime';
      end
      else if CompareText(field.FieldType, 'time') = 0 then
      begin
        v := AReader.FieldByName(field.FieldName).AsDateTime;
        s := field.FieldName + ' as time';
      end
      else
        raise Exception.Create('Unknown field type for ' + field.FieldName);
      try
        TdormUtils.SetField(AObject, field.name, v);
      except
        on E: Exception do
        begin
          raise EdormException.Create(E.Message + sLineBreak + '. Probably cannot write ' +
            ARttiType.ToString + '.' + s);
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

function TZeosDBOBaseAdapter.RawExecute(SQL: string): Int64;
begin
  GetLogger.Warning('RAW EXECUTE: ' + SQL);
  Result := FB.Execute(SQL);
end;

class procedure TZeosDBOBaseAdapter.register;
begin
  //
end;

procedure TZeosDBOBaseAdapter.Rollback;
begin
  FB.GetConnection.Rollback;
end;

procedure TZeosDBOBaseAdapter.SetLogger(ALogger: IdormLogger);
begin
  FLogger := ALogger;
end;

procedure TZeosDBOBaseAdapter.SetZeosDBOParameterValue(AFieldType: string;
  AStatement: TZQuery; ParameterIndex: Integer; AValue: TValue;
  IsNullable: boolean);
var
  sourceStream: TStream;
  str: TBytesStream;
begin
  if CompareText(AFieldType, 'string') = 0 then
  begin
    if IsNullable and CanBeConsideredAsNull(AValue) then
      AStatement.Params[ParameterIndex].Clear
    else
      AStatement.Params[ParameterIndex].AsString := AValue.AsString;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + AValue.AsString);
  end
  else if CompareText(AFieldType, 'decimal') = 0 then
  begin
    if IsNullable and CanBeConsideredAsNull(AValue) then
      AStatement.Params[ParameterIndex].Clear
    else
      AStatement.Params[ParameterIndex].AsFloat := AValue.AsExtended;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + FloatToStr(AValue.AsExtended));
  end
  else if CompareText(AFieldType, 'integer') = 0 then
  begin
    if IsNullable and CanBeConsideredAsNull(AValue) then
      AStatement.Params[ParameterIndex].Clear
    else
      AStatement.Params[ParameterIndex].AsInteger := AValue.AsInteger;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + IntToStr(AValue.AsInt64));
  end
  else if CompareText(AFieldType, 'boolean') = 0 then
  begin
    AStatement.Params[ParameterIndex].AsBoolean := AValue.AsBoolean;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + BoolToStr(AValue.AsBoolean, true));
  end
  else if CompareText(AFieldType, 'date') = 0 then
  begin
    if IsNullable and CanBeConsideredAsNull(AValue) then
      AStatement.Params[ParameterIndex].Clear
    else
      AStatement.Params[ParameterIndex].AsDate := Trunc(AValue.AsExtended);

    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' +
      EscapeDate(Trunc(AValue.AsExtended)));
  end
  else if CompareText(AFieldType, 'datetime') = 0 then
  begin
    if IsNullable and CanBeConsideredAsNull(AValue) then
      AStatement.Params[ParameterIndex].Clear
    else
      AStatement.Params[ParameterIndex].AsDateTime := AValue.AsExtended;

    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + EscapeDate(AValue.AsExtended));
  end
  else if CompareText(AFieldType, 'time') = 0 then
  begin
    if IsNullable and CanBeConsideredAsNull(AValue) then
      AStatement.Params[ParameterIndex].Clear
    else
      AStatement.Params[ParameterIndex].AsDateTime := AValue.AsExtended;

    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + EscapeDateTime(AValue.AsExtended));
  end
  else if CompareText(AFieldType, 'blob') = 0 then
  begin
    sourceStream := TStream(AValue.AsObject);

    if sourceStream = nil then
    begin
      AStatement.Params[ParameterIndex].Clear;
      GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = nil');
    end
    else
    begin
      str := TBytesStream.Create;

      try
        sourceStream.Position := 0;
        str.CopyFrom(sourceStream, 0);
        str.Position := 0;
        AStatement.Params[ParameterIndex].SetBlobData(str.Bytes, str.Size);
        GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = <blob ' + IntToStr(str.Size) +
          ' bytes>');
      finally
        str.Free;
      end;
    end;
  end
  else
    raise EdormException.CreateFmt('Parameter type not supported: [%s]', [AFieldType]);
end;

procedure TZeosDBOBaseAdapter.StartTransaction;
begin
  FB.GetConnection; // ensure database connected
  FB.StartTransaction;
end;

function TZeosDBOBaseAdapter.Update(ARttiType: TRttiType; AObject: TObject;
  AMappingTable: TMappingTable; ACurrentVersion: Int64): Int64;
var
  field: TMappingField;
  SQL: string;
  Query: TZQuery;
  I, pk_idx: Integer;
  v: TValue;
  sql_fields_names: string;
  pk_field: string;
begin
  sql_fields_names := '';

  for field in AMappingTable.Fields do
    if not field.IsPK then
      sql_fields_names := sql_fields_names + ',"' + field.FieldName + '" = :' + Field.FieldName;

  System.Delete(sql_fields_names, 1, 1);
  pk_field := AMappingTable.Fields[GetPKMappingIndex(AMappingTable.Fields)].FieldName;
  SQL := Format('UPDATE %S SET %S WHERE %S = :%S', [AMappingTable.TableName, sql_fields_names,
    pk_field, pk_field]);

  if ACurrentVersion >= 0 then // optlock
    SQL := SQL + ' AND OBJVERSION = ' + IntToStr(ACurrentVersion);
  // else
  // raise EdormLockingException.Create('Invalid ObjVersion');
  GetLogger.Debug(AMappingTable.Fields[GetPKMappingIndex(AMappingTable.Fields)].FieldName);
  GetLogger.Debug('PREPARING: ' + SQL);
  Query := FB.Prepare(SQL);

  try
    I := 0;

    for field in AMappingTable.Fields do
    begin
      v := TdormUtils.GetField(AObject, field.name);
      if field.IsPK then
        Continue
      else
      begin
        SetZeosDBOParameterValue(field.FieldType, Query, I, v, field.Nullable);
      end;
      inc(I);
    end;
    pk_idx := GetPKMappingIndex(AMappingTable.Fields);
    v := ARttiType.GetProperty(AMappingTable.Fields[pk_idx].name).GetValue(AObject);
    FillPrimaryKeyParam(Query, I, v);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    Result := FB.Execute(Query);
  finally
    Query.Free;
  end;
end;

{ TZeosDBOBaseTableSequence }

function TZeosDBOBaseTableSequence.NewIntegerKey(const Entity: string): UInt64;
var
  SequenceName: string;
begin
  SequenceName := Format(GetSequenceFormatTemplate, [Entity]);
  Result := FPersistStrategy.ExecuteAndGetFirst('SELECT GEN_ID(' + SequenceName +
    ',1) FROM RDB$DATABASE');
end;

function TZeosDBOBaseTableSequence.NewStringKey(const Entity: string): string;
var
  GUID: TGUID;
begin
  //raise EdormException.Create('String keys not supported');
  CreateGUID(GUID);
  Result := GUIDToString(GUID);
end;

procedure TZeosDBOBaseTableSequence.SetPersistStrategy(
  const PersistentStrategy: IdormPersistStrategy);
begin
  FPersistStrategy := PersistentStrategy;
end;

end.
