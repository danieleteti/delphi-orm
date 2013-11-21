unit dorm.adapter.FireDac.BaseAdapter;

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
  dorm.adapter.FireDac.Facade,
  uADGUIxIntf, uADGUIxFormsWait,
  uADStanIntf, uADStanOption, uADStanError, uADPhysIntf, uADStanDef,
  uADStanPool, uADStanAsync, uADPhysManager, uADStanParam, uADDatSManager,
  uADDAptIntf, uADDAptManager, uADCompClient, uADCompDataSet,
  uADCompGUIx,
  dorm.adapter.Base,
  dorm.Mappings.Strategies, dialogs;

type
  TFireDacBaseAdapter = class(TBaseAdapter, IdormPersistStrategy)
  strict private
    function Load(ARttiType: TRttiType; ATableName: string; AMappingTable: TMappingTable;
      const Value: TValue): TObject; overload;

  private
    function GetFireDacReaderFor(ARttiType: TRttiType; AMappingTable: TMappingTable;
      const Value: TValue; AMappingRelationField: TMappingField = nil): TADQuery;

  protected
    FFormatSettings        : TFormatSettings;
    FD                     : TFireDacFacade;
    FLogger                : IdormLogger;
    FKeysGeneratorClassName: string;
    FKeysGenerator         : IdormKeysGenerator;
    FKeyType               : TdormKeyType;
    FNullKeyValue          : TValue;
    FLastInsertOID         : TValue;
    procedure InitFormatSettings;
    function CreateFireDacFacade(Conf: ISuperObject): TFireDacFacade; virtual; abstract;
    function CreateObjectFromFireDacQuery(ARttiType: TRttiType; AReader: TADQuery;
      AMappingTable: TMappingTable): TObject;
    procedure LoadObjectFromDBXReader(AObject: TObject; ARttiType: TRttiType; AReader: TADQuery;
      AFieldsMapping: TMappingFieldList);
    function GetLogger: IdormLogger;
    procedure SetFireDacParameterValue(AFieldType: string; AStatement: TADQuery;
      ParameterIndex: Integer; AValue: TValue);
    /// <summary>
    ///    This function indicates whether it is necessary, during insertion of the record,
    ///    calculate the primary key or if the value assignment is executed by the database.
    ///    Is overridden by the adapter
    /// </summary>
    function CalculatePrimaryKey: Boolean; virtual;
  public
    function GetGeneratedPrimaryKey: TValue;
    function GenerateAndFillPrimaryKeyParam(Query: TADQuery; ParamIndex: Integer;
      const Entity: string): TValue; overload;
    function FillPrimaryKeyParam(Query: TADQuery; ParamIndex: Integer;
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
      : Boolean; overload;
    function Load(ARttiType: TRttiType; AMappingTable: TMappingTable; const Value: TValue;
      AObject: TObject): Boolean; overload;
    function List(ARttiType: TRttiType; AMappingTable: TMappingTable; ACriteria: ICriteria)
      : TObjectList<TObject>;
    procedure LoadList(AList: TObject; ARttiType: TRttiType; AMappingTable: TMappingTable;
      ACriteria: ICriteria); overload;
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
    function GetDatabaseBuilder(AEntities: TList<string>; AMappings: ICacheMappingStrategy)
      : IDataBaseBuilder;
    function ExecuteCommand(ACommand: IdormCommand): Int64;
  end;

  TFireDacBaseTableSequence = class abstract(TdormInterfacedObject, IdormKeysGenerator)
  protected
    FPersistStrategy: IdormPersistStrategy;
    function GetSequenceFormatTemplate: string; virtual; abstract;

  public
    function NewStringKey(const Entity: string): string;
    function NewIntegerKey(const Entity: string): UInt64; virtual;
    procedure SetPersistStrategy(const PersistentStrategy: IdormPersistStrategy);
  end;

implementation

uses
  dorm.Utils;

procedure TFireDacBaseAdapter.InitFormatSettings;
begin
  FFormatSettings.LongDateFormat := 'YYYY-MM-DD';
  FFormatSettings.ShortDateFormat := 'YYYY-MM-DD';
  FFormatSettings.LongTimeFormat := 'HH:NN:SS';
  FFormatSettings.ShortTimeFormat := 'HH:NN:SS';
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.TimeSeparator := ':';
end;

function TFireDacBaseAdapter.Update(ARttiType: TRttiType; AObject: TObject;
  AMappingTable: TMappingTable; ACurrentVersion: Int64): Int64;
var
  field           : TMappingField;
  SQL             : string;
  Query           : TADQuery;
  I, pk_idx       : Integer;
  v               : TValue;
  sql_fields_names: string;
  pk_field        : string;
begin
  sql_fields_names := '';
  for field in AMappingTable.Fields do
    if not field.IsPK then
      sql_fields_names := sql_fields_names + ',"' + field.FieldName + '" = ?';
  System.Delete(sql_fields_names, 1, 1);
  pk_field := AMappingTable.Fields[GetPKMappingIndex(AMappingTable.Fields)].FieldName;
  SQL := Format('UPDATE %S SET %S WHERE %S = ?', [AMappingTable.TableName, sql_fields_names,
    pk_field]);
  if ACurrentVersion >= 0 then // optlock
    SQL := SQL + ' AND OBJVERSION = ' + IntToStr(ACurrentVersion);
  GetLogger.Debug(AMappingTable.Fields[GetPKMappingIndex(AMappingTable.Fields)].FieldName);
  GetLogger.Debug('PREPARING: ' + SQL);
  Query := FD.Prepare(SQL);
  try
    I := 0;
    for field in AMappingTable.Fields do
    begin
      v := TdormUtils.GetField(AObject, field.name);
      if field.IsPK then
        Continue
      else
      begin
        SetFireDacParameterValue(field.FieldType, Query, I, v);
      end;
      inc(I);
    end;
    pk_idx := GetPKMappingIndex(AMappingTable.Fields);
    v := ARttiType.GetProperty(AMappingTable.Fields[pk_idx].name).GetValue(AObject);
    FillPrimaryKeyParam(Query, I, v);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    Result := FD.Execute(Query);
  finally
    Query.Free;
  end;
end;

function TFireDacBaseAdapter.CalculatePrimaryKey: Boolean;
begin
   Result:=False;
end;

procedure TFireDacBaseAdapter.Commit;
begin
  FD.CommitTransaction;
end;

procedure TFireDacBaseAdapter.ConfigureStrategy(ConfigurationInfo: ISuperObject);
var
  ctx: TRttiContext;
  t  : TRttiType;
  obj: TObject;
begin
  FD := CreateFireDacFacade(ConfigurationInfo);
  FKeysGeneratorClassName := ConfigurationInfo.S['keys_generator'];
  t := ctx.FindType(FKeysGeneratorClassName);
  if t = nil then
    raise EdormException.Create('Unknown key generator ' + FKeysGeneratorClassName);
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

function TFireDacBaseAdapter.Count(AMappingTable: TMappingTable): Int64;
var
  cmd: TADQuery;
  SQL: string;
begin
  Result := - 1;
  SQL := 'SELECT COUNT(*) FROM ' + AMappingTable.TableName;
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FD.Prepare(SQL);
  try
    cmd.Open;
    if not cmd.Eof then
      Result := cmd.Fields[0].AsInteger;
  finally
    cmd.Free;
  end;
end;

function TFireDacBaseAdapter.Delete(ARttiType: TRttiType; AObject: TObject;
  AMappingTable: TMappingTable; ACurrentVersion: Int64): Int64;
var
  pk_idx                               : Integer;
  pk_value                             : TValue;
  pk_attribute_name, pk_field_name, SQL: string;
  cmd                                  : TADQuery;
  S                                    : Cardinal;
  I                                    : Cardinal;
  u                                    : Cardinal;
  d                                    : Cardinal;
begin
  pk_idx := GetPKMappingIndex(AMappingTable.Fields);
  if pk_idx = - 1 then
    raise Exception.Create('Invalid primary key for table ' + AMappingTable.TableName);
  pk_attribute_name := AMappingTable.Fields[pk_idx].name;
  pk_field_name := AMappingTable.Fields[pk_idx].FieldName;
  pk_value := ARttiType.GetProperty(pk_attribute_name).GetValue(AObject);
  SQL := 'DELETE FROM ' + AMappingTable.TableName + ' WHERE ' + pk_field_name + ' = ?';
  if ACurrentVersion >= 0 then
    SQL := SQL + ' AND OBJVERSION = ' + IntToStr(ACurrentVersion);
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FD.Prepare(SQL);
  try
    FillPrimaryKeyParam(cmd, 0, pk_value);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    Result:=FD.ExecuteQuery(cmd).RowsAffected;
{    cmd.AffectedRows(S, I, u, d);
    Result := d;}
  finally
    cmd.Free;
  end;
end;

procedure TFireDacBaseAdapter.DeleteAll(AMappingTable: TMappingTable);
var
  SQL: string;
begin
  SQL := 'DELETE FROM ' + AMappingTable.TableName;
  GetLogger.Debug('EXECUTING :' + SQL);
  FD.Execute(SQL);
end;

destructor TFireDacBaseAdapter.Destroy;
begin
  FD.Free;
  inherited;
end;

function TFireDacBaseAdapter.ExecuteAndGetFirst(SQL: string): Int64;
var
  cmd: TADQuery;
begin
  GetLogger.EnterLevel('ExecuteAndGetFirst');
  Result := 0;
  GetLogger.Info('PREPARING: ' + SQL);
  cmd := FD.Prepare(SQL);
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

function TFireDacBaseAdapter.ExecuteCommand(ACommand: IdormCommand): Int64;
var
  SQL           : string;
  reader        : TADQuery;
  CustomCriteria: ICustomCriteria;
  sr            : Cardinal;
  ir            : Cardinal;
  ur            : Cardinal;
  dr            : Cardinal;
begin
  SQL := ACommand.GetSQL;
  GetLogger.Debug('EXECUTING: ' + SQL);
  reader := FD.Prepare(SQL);
  try
    if reader.Params.Count <> 0 then
      raise EdormException.Create('Parameters not replaced');
    Result:=FD.ExecuteQuery(reader).RowsAffected;
    {reader.AffectedRows(sr, ir, ur, dr);
    Result := ir + ur + dr;}
  finally
    reader.Free;
  end;
end;

function TFireDacBaseAdapter.GenerateAndFillPrimaryKeyParam(Query: TADQuery; ParamIndex: Integer;
  const Entity: string): TValue;
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

function TFireDacBaseAdapter.FillPrimaryKeyParam(Query: TADQuery; ParamIndex: Integer;
  const Value: TValue): TValue;
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
          Query.Params[ParamIndex].AsInteger:= Value.AsInteger;
          Result := Query.Params[ParamIndex].AsInteger;
          GetLogger.Debug('ParPK = ' + IntToStr(Value.AsInteger));
        end;
    end;
  except
    on E: Exception do
      raise EdormException.Create('Error during fill primary key for query. ' + E.Message);
  end;
end;

function TFireDacBaseAdapter.GetDatabaseBuilder(AEntities: TList<string>;
  AMappings: ICacheMappingStrategy): IDataBaseBuilder;
begin
  AEntities.Free; // just to hide the memory leak
  raise Exception.Create('Not implemented for ' + self.ClassName);
end;

function TFireDacBaseAdapter.GetKeysGenerator: IdormKeysGenerator;
begin
  Result := FKeysGenerator;
end;

function TFireDacBaseAdapter.GetKeyType: TdormKeyType;
begin
  Result := FKeyType;
end;

function TFireDacBaseAdapter.GetLastInsertOID: TValue;
begin
  Result := FLastInsertOID;
end;

function TFireDacBaseAdapter.GetLogger: IdormLogger;
begin
  Result := FLogger;
end;

procedure TFireDacBaseAdapter.InitStrategy;
begin
  FLastInsertOID := TValue.Empty;
end;

function TFireDacBaseAdapter.Insert(ARttiType: TRttiType; AObject: TObject;
  AMappingTable: TMappingTable): TValue;
var
  field                                   : TMappingField;
  sql_fields_names, sql_fields_values, SQL: ansistring;
  Query                                   : TADQuery;
  I, pk_idx                               : Integer;
  v, pk_value                             : TValue;
begin
  sql_fields_names := '';
  for field in AMappingTable.Fields do
  begin { todo: manage transients fields }
     if (not field.IsPK) or (field.IsPK and CalculatePrimaryKey) then
        sql_fields_names := sql_fields_names + ',"' + ansistring(field.FieldName) + '"';
  end;
  System.Delete(sql_fields_names, 1, 1);
  sql_fields_values := '';
  for I := 0 to AMappingTable.Fields.Count - 1 do
  begin
     if (not AMappingTable.Fields[i].IsPK) or (AMappingTable.Fields[i].IsPK and CalculatePrimaryKey) then
        sql_fields_values := sql_fields_values + ',?';
  end;
  System.Delete(sql_fields_values, 1, 1);
  SQL := Format('INSERT INTO %s (%S) VALUES (%S)', [AMappingTable.TableName, sql_fields_names,
    sql_fields_values]);
  GetLogger.Debug('PREPARING :' + string(SQL));
  Query := FD.Prepare(string(SQL));
  try
    I := 0;
    for field in AMappingTable.Fields do
    begin
      v := TdormUtils.GetField(AObject, field.RTTICache);
      if field.IsPK then
      begin
        if CalculatePrimaryKey then
        begin
          pk_value := GenerateAndFillPrimaryKeyParam(Query, I, AMappingTable.TableName);
          inc(I);
        end;
      end
      else
      begin
        SetFireDacParameterValue(field.FieldType, Query, I, v);
        inc(I);
      end;
    end;
    GetLogger.Debug('EXECUTING PREPARED :' + string(SQL));
    FD.Execute(Query);
  finally
    Query.Free;
  end;
  pk_idx := GetPKMappingIndex(AMappingTable.Fields);

  if not CalculatePrimaryKey then
     pk_value := GetGeneratedPrimaryKey;

  TdormUtils.SetField(AObject, AMappingTable.Fields[pk_idx].RTTICache, pk_value);
  Result := pk_value;
end;

function TFireDacBaseAdapter.InTransaction: Boolean;
var
  tr: TADTransaction;
begin
  tr := FD.GetCurrentTransaction;
  Result := assigned(tr);
  if Result then
    Result := tr.Connection.InTransaction
end;

function TFireDacBaseAdapter.IsNullKey(const Value: TValue): Boolean;
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

function TFireDacBaseAdapter.GetNullKeyValue: TValue;
begin
  Result := FNullKeyValue;
end;

function TFireDacBaseAdapter.List(ARttiType: TRttiType; AMappingTable: TMappingTable;
  ACriteria: ICriteria): TObjectList<TObject>;
begin
  Result := NewList();
  LoadList(Result, ARttiType, AMappingTable, ACriteria);
end;

function TFireDacBaseAdapter.Load(ARttiType: TRttiType; ATableName: string;
  AMappingTable: TMappingTable; const Value: TValue): TObject;
var
  pk_idx                               : Integer;
  pk_attribute_name, pk_field_name, SQL: string;
  cmd                                  : TADQuery;
begin
  Result := nil;
  pk_idx := GetPKMappingIndex(AMappingTable.Fields);
  if pk_idx = - 1 then
    raise Exception.Create('Invalid primary key for table ' + ATableName);
  pk_attribute_name := AMappingTable.Fields[pk_idx].name;
  pk_field_name := AMappingTable.Fields[pk_idx].FieldName;
  SQL := 'SELECT ' + GetSelectFieldsList(AMappingTable.Fields, true) + ' FROM ' + ATableName +
    ' WHERE ' + pk_field_name + ' = ?';
  GetLogger.Debug('PREPARING: ' + SQL);
  cmd := FD.Prepare(SQL);
  try
    FillPrimaryKeyParam(cmd, pk_idx, Value);
    GetLogger.Debug('EXECUTING PREPARED: ' + SQL);
    cmd.Open;
    if not cmd.Eof then
      Result := CreateObjectFromFireDacQuery(ARttiType, cmd, AMappingTable);
  finally
    cmd.Free;
  end;
end;

function TFireDacBaseAdapter.GetFireDacReaderFor(ARttiType: TRttiType; AMappingTable: TMappingTable;
  const Value: TValue; AMappingRelationField: TMappingField): TADQuery;
var
  pk_idx            : Integer;
  pk_field_name, SQL: string;
begin
  if AMappingRelationField = nil then
  begin
    pk_idx := GetPKMappingIndex(AMappingTable.Fields);
    if pk_idx = - 1 then
      raise Exception.Create('Invalid primary key for table ' + AMappingTable.TableName);
    pk_field_name := AMappingTable.Fields[pk_idx].FieldName;
    SQL := 'SELECT ' + GetSelectFieldsList(AMappingTable.Fields, true) + ' FROM ' +
      AMappingTable.TableName + ' WHERE ' + pk_field_name + ' = :' + pk_field_name;
  end
  else
  begin
    pk_idx := GetPKMappingIndex(AMappingTable.Fields);
    if pk_idx = - 1 then
      raise Exception.Create('Invalid primary key for table ' + AMappingTable.TableName);
    pk_field_name := AMappingTable.Fields[pk_idx].FieldName;
    SQL := 'SELECT ' + GetSelectFieldsList(AMappingTable.Fields, true) + ' FROM ' +
      AMappingTable.TableName + ' WHERE ' + AMappingRelationField.FieldName + ' = :' +
      pk_field_name;
  end;
  GetLogger.Debug('PREPARING: ' + SQL);
  Result := FD.Prepare(SQL);
  FillPrimaryKeyParam(Result, 0, Value);
end;

function TFireDacBaseAdapter.GetGeneratedPrimaryKey: TValue;
begin
  Result := Int64(FKeysGenerator.NewIntegerKey(''));
  FLastInsertOID := Result;
end;

function TFireDacBaseAdapter.Load(ARttiType: TRttiType; AMappingTable: TMappingTable;
  AMappingRelationField: TMappingField; const Value: TValue; AObject: TObject): Boolean;
var
  reader: TADQuery;
begin
  reader := GetFireDacReaderFor(ARttiType, AMappingTable, Value, AMappingRelationField);
  try
    reader.Open();
    Result := not reader.Eof;
    if Result then
      LoadObjectFromDBXReader(AObject, ARttiType, reader, AMappingTable.Fields);
    reader.Next;
    if not reader.Eof then
      // there is some problem.... here I should have only one record
      raise EdormException.Create('Singleton select returns more than 1 record');
  finally
    reader.Free;
  end;
end;

procedure TFireDacBaseAdapter.LoadList(AList: TObject; ARttiType: TRttiType;
  AMappingTable: TMappingTable; ACriteria: ICriteria);
var
  SQL           : string;
  reader        : TADQuery;
  CustomCriteria: ICustomCriteria;
begin
  if assigned(ACriteria) and TInterfacedObject(ACriteria).GetInterface(ICustomCriteria,
    CustomCriteria) then
    SQL := CustomCriteria.GetSQL
  else
    SQL := self.GetSelectSQL(ACriteria, AMappingTable);
  GetLogger.Debug('EXECUTING: ' + SQL);
  //SQL:='SELECT "ID","FIRST_NAME","LAST_NAME","AGE","BORN_DATE","BORN_DATE_TIME","PHOTO","IS_MALE","OBJVERSION" FROM PEOPLE WHERE BORN_DATE = TO_DATE (''20.10.2000'', ''DD.MM.YYYY'')';
  //Modifica temporanea per test. Lorenzo Caldon
  //SQL:='SELECT "ID","FIRST_NAME","LAST_NAME","AGE","BORN_DATE","BORN_DATE_TIME","PHOTO","IS_MALE","OBJVERSION" FROM PEOPLE WHERE BORN_DATE = :BORN_DATE';
  reader := FD.Prepare(SQL);
  //reader.ParamByName('BORN_DATE').AsDateTime:=EncodeDate(2000,10,20);

  if reader.Params.Count <> 0 then
    raise EdormException.Create('Parameters not replaced');
  reader.Active:=True;
  try
    while not reader.Eof do
    begin
      TdormUtils.MethodCall(AList, 'Add', [CreateObjectFromFireDacQuery(ARttiType, reader,
        AMappingTable)]);
      reader.Next;
    end;
  finally
    reader.Free;
  end;
end;

procedure TFireDacBaseAdapter.LoadObjectFromDBXReader(AObject: TObject; ARttiType: TRttiType;
  AReader: TADQuery; AFieldsMapping: TMappingFieldList);
var
  field       : TMappingField;
  v           : TValue;
  S           : string;
  sourceStream: TStream;
  Stream: TStream;
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
        // targetStream := nil;
        S := field.FieldName + ' as blob';
        sourceStream := nil;
        if not AReader.FieldByName(field.FieldName).IsNull then
        begin
          sourceStream := TMemoryStream.Create;
          Stream:=AReader.CreateBlobStream(AReader.FieldByName(field.FieldName), bmRead);
          try
             Stream.Position:=0;
             sourceStream.CopyFrom(Stream, Stream.Size);
          finally
             Stream.Free;
          end;
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
        v := (AReader.FieldByName(field.FieldName).AsInteger=1);
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
      else
        raise Exception.Create('Unknown field type for ' + field.FieldName);
      try
        TdormUtils.SetField(AObject, field.name, v);
      except
        on E: Exception do
        begin
          raise EdormException.Create(E.Message + sLineBreak + '. Probably cannot write ' +
            ARttiType.ToString + '.' + S);
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

function TFireDacBaseAdapter.RawExecute(SQL: string): Int64;
begin
  GetLogger.Warning('RAW EXECUTE: ' + SQL);
  Result := FD.Execute(SQL);
end;

function TFireDacBaseAdapter.CreateObjectFromFireDacQuery(ARttiType: TRttiType; AReader: TADQuery;
  AMappingTable: TMappingTable): TObject;
var
  obj         : TObject;
  field       : TMappingField;
  v           : TValue;
  S           : string;
  targetStream: TMemoryStream;
  Stream: TStream;
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
          targetStream := TMemoryStream.Create;
          Stream:=AReader.CreateBlobStream(AReader.FieldByName(field.FieldName), bmRead);
          try
             Stream.Position:=0;
             targetStream.CopyFrom(Stream, Stream.Size);
          finally
             Stream.Free;
          end;
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
      else if CompareText(field.FieldType, 'boolean') = 0 then
      begin
        v := (AReader.FieldByName(field.FieldName).AsInteger=1);
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
          raise EdormException.Create(E.Message + sLineBreak + '. Probably cannot write ' +
            ARttiType.ToString + '.' + S);
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

class procedure TFireDacBaseAdapter.register;
begin
  //
end;

procedure TFireDacBaseAdapter.Rollback;
begin
  FD.GetCurrentTransaction.Rollback;
end;

procedure TFireDacBaseAdapter.SetFireDacParameterValue(AFieldType: string; AStatement: TADQuery;
  ParameterIndex: Integer; AValue: TValue);
var
  sourceStream: TStream;
  str         : TBytesStream;
begin
  if CompareText(AFieldType, 'string') = 0 then
  begin
    AStatement.Params[ParameterIndex].AsString := AValue.AsString;
    AStatement.Params[ParameterIndex].DataType:=ftString;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + AValue.AsString);
  end
  else if CompareText(AFieldType, 'decimal') = 0 then
  begin
    AStatement.Params[ParameterIndex].AsFloat := AValue.AsExtended;
    AStatement.Params[ParameterIndex].DataType:=ftFloat;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + FloatToStr(AValue.AsExtended));
  end
  else if CompareText(AFieldType, 'integer') = 0 then
  begin
    AStatement.Params[ParameterIndex].AsInteger := AValue.AsInt64;
    AStatement.Params[ParameterIndex].DataType:=ftInteger;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + IntToStr(AValue.AsInt64));
  end
  else if CompareText(AFieldType, 'boolean') = 0 then
  begin
    if AValue.AsBoolean then
       AStatement.Params[ParameterIndex].AsInteger := 1
    else
       AStatement.Params[ParameterIndex].AsInteger := 0;
    AStatement.Params[ParameterIndex].DataType:=ftInteger;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + BoolToStr(AValue.AsBoolean, true));
  end
  else if CompareText(AFieldType, 'date') = 0 then
  begin
    AStatement.Params[ParameterIndex].AsDateTime := trunc(AValue.AsExtended);
    AStatement.Params[ParameterIndex].DataType:=ftDateTime;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' +
      EscapeDate(trunc(AValue.AsExtended)));
  end
  else if CompareText(AFieldType, 'datetime') = 0 then
  begin
    AStatement.Params[ParameterIndex].AsDateTime := AValue.AsExtended;
    AStatement.Params[ParameterIndex].DataType:=ftDateTime;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + EscapeDate(AValue.AsExtended));
  end
  else if CompareText(AFieldType, 'time') = 0 then
  begin
    AStatement.Params[ParameterIndex].AsDateTime := AValue.AsExtended;
    AStatement.Params[ParameterIndex].DataType:=ftDateTime;
    GetLogger.Debug('Par' + IntToStr(ParameterIndex) + ' = ' + EscapeDateTime(AValue.AsExtended));
  end
  else if CompareText(AFieldType, 'blob') = 0 then
  begin
    sourceStream := TStream(AValue.AsObject);
    AStatement.Params[ParameterIndex].DataType:=ftBlob;
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
        AStatement.Params[ParameterIndex].LoadFromStream(sourceStream, ftBlob);
        AStatement.Params[ParameterIndex].DataType:=ftBlob;
        {TODO --oLorenzo -cblob : sistemare}
        //AStatement.ParamsSetBlob(ParameterIndex, str);
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

procedure TFireDacBaseAdapter.SetLogger(ALogger: IdormLogger);
begin
  FLogger := ALogger;
end;

procedure TFireDacBaseAdapter.StartTransaction;
begin
  FD.GetConnection; // ensure database connected
  FD.StartTransaction;
end;
{ TFirebirdFireDacBaseTableSequence }

function TFireDacBaseTableSequence.NewIntegerKey(const Entity: string): UInt64;
var
  SequenceName: string;
begin
  SequenceName := Format(GetSequenceFormatTemplate, [Entity]);
  Result := FPersistStrategy.ExecuteAndGetFirst('SELECT GEN_ID(' + SequenceName +
    ',1) FROM RDB$DATABASE');
end;

function TFireDacBaseTableSequence.NewStringKey(const Entity: string): string;
begin
  raise EdormException.Create('String keys not supported');
end;

procedure TFireDacBaseTableSequence.SetPersistStrategy(const PersistentStrategy: IdormPersistStrategy);
begin
  FPersistStrategy := PersistentStrategy;
end;

function TFireDacBaseAdapter.Load(ARttiType: TRttiType; AMappingTable: TMappingTable;
  const Value: TValue; AObject: TObject): Boolean;
var
  reader: TADQuery;
begin
  reader := GetFireDacReaderFor(ARttiType, AMappingTable, Value);
  try
    reader.Open();
    Result := not reader.Eof;
    if Result then
      LoadObjectFromDBXReader(AObject, ARttiType, reader, AMappingTable.Fields);
  finally
    reader.Free;
  end;
end;

end.
