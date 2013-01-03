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
unit dorm;

interface

uses
  ioutils,
  classes,
  superobject,
  Generics.Collections,
  TypInfo,
  Rtti,
  dorm.Filters,
  dorm.Collections,
  dorm.UOW,
  dorm.Mappings.Strategies,
  dorm.Mappings,
  dorm.Commons;

type

{$IF CompilerVersion = 22}

  // In DelphiXE you cannot use directly TObjectList<T> because
  // GetItem method is private. SO you have to use a specific typed list
  // as this sample show. However, you can avoid default delphi collections
  // and use whatever list you want. The only requirements are the following methods:
  // - function Add(Object: TObject)
  // - procedure Clear
  // - property Count: Integer
  // - function GetItem(Index: Integer): TObject
type
  TdormObjectList<T: class> = class(TObjectList<T>)
  public
    function GetElement(Index: Integer): T;
  end;

{$IFEND}

  TSession = class(TdormInterfacedObject)
  private
    FMappingStrategy: ICacheMappingStrategy;
    FCTX: TRttiContext;
    FConfig: ISuperObject;
    FIdType: TdormKeyType;
    FIdNullValue: TValue;
    FPersistStrategy: IdormPersistStrategy;
    FLogger: IdormLogger;
    FEnvironment: TdormEnvironment;
    EnvironmentNames: TArray<string>;
    FLoadEnterExitCounter: Integer;
    LoadedObjects: TObjectDictionary<string, TObject>;
    procedure LoadEnter;
    procedure LoadExit;
    function GetIdValue(AIdMappingField: TMappingField;
      AObject: TObject): TValue;
    procedure ReadIDConfig(const AJsonPersistenceConfig: ISuperObject);
    /// this method load an object using a value and a specific mapping field. is used to load the
    // relations between objects
    function LoadByMappingField(ATypeInfo: PTypeInfo;
      AMappingField: TMappingField; const Value: TValue): TObject; overload;
    function LoadByMappingField(ATypeInfo: PTypeInfo;
      AMappingField: TMappingField; const Value: TValue; AObject: TObject)
      : boolean; overload;

  strict protected
    CurrentObjectStatus: TdormObjectStatus;
    FValidatingDuck: TDuckTypedObject;
    procedure UpdateChildTypeWithRealListInnerType(AMappingTable: TMappingTable;
      var AChildType: TRttiType);

  protected
    constructor CreateSession(Environment: TdormEnvironment); virtual;
    // Validations
    function CreateLogger: IdormLogger;
    function Qualified(AMappingTable: TMappingTable;
      const AClassName: string): string;
    function GetPackageName(AMappingTable: TMappingTable;
      const AClassName: string): string;
    function GetStrategy: IdormPersistStrategy;
    procedure InsertHasManyRelation(AMappingTable: TMappingTable;
      AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
    procedure PersistHasManyRelation(AMappingTable: TMappingTable;
      AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
    procedure PersistHasOneRelation(AMappingTable: TMappingTable;
      AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
    procedure InsertHasOneRelation(AMappingTable: TMappingTable;
      AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
    procedure FixBelongsToRelation(AMappingTable: TMappingTable;
      AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
    procedure DeleteHasManyRelation(AMappingTable: TMappingTable;
      AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
    procedure DeleteHasOneRelation(AMappingTable: TMappingTable;
      AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
    procedure LoadHasManyRelation(ATableMapping: TMappingTable;
      AIdValue: TValue; ARttiType: TRttiType; AObject: TObject;
      AConsiderLazyLoading: boolean = true);
    procedure LoadHasManyRelationByPropertyName(AIdValue: TValue;
      ARttiType: TRttiType; AClassName: string; APropertyName: string;
      var AObject: TObject);
    procedure LoadHasOneRelation(ATableMapping: TMappingTable; AIdValue: TValue;
      ARttiType: TRttiType; AObject: TObject;
      AConsiderLazyLoading: boolean = true);
    procedure LoadBelongsToRelation(ATableMapping: TMappingTable;
      AIdValue: TValue; ARttiType: TRttiType; AObject: TObject;
      AConsiderLazyLoading: boolean = true);
    procedure LoadHasOneRelationByPropertyName(AIdValue: TValue;
      ARttiType: TRttiType; AClassName: string; APropertyName: string;
      var AObject: TObject);
    procedure LoadBelongsToRelationByPropertyName(AIdValue: TValue;
      ARttiType: TRttiType; AClassName, APropertyName: string;
      var AObject: TObject);
    function Load(ATypeInfo: PTypeInfo; const Value: TValue): TObject; overload;
    // Internal use
    function Load(ATypeInfo: PTypeInfo; const Value: TValue; AObject: TObject)
      : boolean; overload;
    procedure SetLazyLoadFor(ATypeInfo: PTypeInfo; const APropertyName: string;
      const Value: boolean);
    // function FindOne(AItemClassInfo: PTypeInfo; Criteria: ICriteria;
    // FillOptions: TdormFillOptions = []; FreeCriteria: Boolean = true)
    // : TObject; overload; deprecated;
    function IsNullKey(ATableMap: TMappingTable; const AValue: TValue): boolean;
    ///
    procedure AddAsLoadedObject(AObject: TObject;
      AMappingField: TMappingField); overload;
    procedure AddAsLoadedObject(ACollection: IWrappedList;
      AMappingField: TMappingField); overload;
    function IsAlreadyLoaded(ATypeInfo: PTypeInfo; AValue: TValue;
      out AOutObject: TObject): boolean;
    function GetLoadedObjectHashCode(AObject: TObject;
      AMappingField: TMappingField): String; overload;
    function GetLoadedObjectHashCode(ATypeInfo: PTypeInfo; AValue: TValue)
      : String; overload;
    procedure InternalUpdate(AObject: TObject;
      AValidaetable: TdormValidateable; AOnlyChild: boolean = false);
    function InternalInsert(AObject: TObject;
      AValidaetable: TdormValidateable): TValue;
    procedure InternalDelete(AObject: TObject;
      AValidaetable: TdormValidateable);

  public
    destructor Destroy; override;
    // Environments
    function GetEnv: string;
    // Utils
    procedure BuildDatabase;
    function Clone<T: class, constructor>(Obj: T): T;
    procedure CopyObject(SourceObject, TargetObject: TObject);
    function GetLogger: IdormLogger;
    function Strategy: IdormPersistStrategy;
    function OIDIsSet(AObject: TObject): boolean;
    procedure ClearOID(AObject: TObject);
    // Configuration
    procedure Configure(APersistenceConfiguration: TTextReader;
      AMappingConfiguration: TTextReader = nil;
      AOwnPersistenceConfigurationReader: boolean = true;
      AOwnMappingConfigurationReader: boolean = true);
    class function CreateConfigured(APersistenceConfiguration: TTextReader;
      AMappingConfiguration: TTextReader; AEnvironment: TdormEnvironment)
      : TSession; overload;
    class function CreateConfigured(APersistenceConfiguration: TTextReader;
      AEnvironment: TdormEnvironment): TSession; overload;
    class function CreateConfigured(APersistenceConfigurationFile: String;
      AEnvironment: TdormEnvironment): TSession; overload;
    // Persistence
    procedure Persist(AObject: TObject);
    procedure HandleDirtyPersist(AIdValue: TValue; AObject: TObject;
      AValidateable: TdormValidateable);
    function Insert(AObject: TObject): TValue; overload;
    function Save(AObject: TObject): TValue; overload;
      deprecated 'Use Insert instead';
    function InsertAndFree(AObject: TObject): TValue;
    procedure Update(AObject: TObject);
    procedure UpdateAndFree(AObject: TObject);
    procedure Save(dormUOW: TdormUOW); overload;
    procedure Delete(AObject: TObject);
    procedure DeleteAndFree(AObject: TObject);
    procedure PersistCollection(ACollection: TObject); overload;
    procedure PersistCollection(ACollection: IWrappedList); overload;
    procedure InsertCollection(ACollection: TObject);
    procedure UpdateCollection(ACollection: TObject);
    procedure DeleteCollection(ACollection: TObject);
    function GetObjectStatus(AObject: TObject): TdormObjectStatus;
    function IsObjectStatusAvailable(AObject: TObject): boolean;
    procedure SetObjectStatus(AObject: TObject; AStatus: TdormObjectStatus;
      ARaiseExceptionIfNotExists: boolean = true);
    function IsDirty(AObject: TObject): boolean;
    function IsClean(AObject: TObject): boolean;
    function IsUnknown(AObject: TObject): boolean;
    procedure LoadRelations(AObject: TObject;
      ARelationsSet: TdormRelations = [drBelongsTo, drHasMany, drHasOne];
      AConsiderLazyLoading: boolean = true); overload;
    procedure LoadRelationsForEachElement(AList: TObject;
      ARelationsSet: TdormRelations = [drBelongsTo, drHasMany, drHasOne];
      AConsiderLazyLoading: boolean = true); overload;
    { Load 0 or 1 object by OID (first parameter). The Session will create and returned object of type <T> }
    function Load<T: class>(const Value: TValue): T; overload;
    { Load 0 or 1 object by OID (first parameter). The Session doesn't create the object, just fill the instance passed on second parameter. This function return true if the OID was found in database. }
    function Load<T: class>(const Value: TValue; AObject: TObject)
      : boolean; overload;
    { Load 0 or 1 object by Criteria. The Session will create and returned object of type <T> }
    function Load<T: class>(ACriteria: ICriteria): T; overload;
    { Load all the objects that satisfy the Criteria. The Session will fill the list (ducktype) passed on 2nd parameter with objects of type <T> }
    procedure LoadList<T: class>(Criteria: ICriteria;
      AObject: TObject); overload;
    { Load all the objects that satisfy the Criteria. The Session will create and return a TObjectList with objects of type <T> }
    function LoadList<T: class>(Criteria: ICriteria = nil):

{$IF CompilerVersion > 22}TObjectList<T>{$ELSE}TdormObjectList<T>{$IFEND};
      overload;
    procedure EnableLazyLoad(AClass: TClass; const APropertyName: string);
    procedure DisableLazyLoad(AClass: TClass; const APropertyName: string);
    function Count(AClassType: TClass; ACriteria: ICriteria = nil): Int64;
    procedure DeleteAll(AClassType: TClass);
    // Find and List
    // function FindOne<T: class>(Criteria: TdormCriteria;
    // FillOptions: TdormFillOptions = []; FreeCriteria: Boolean = true): T; overload; deprecated;
    procedure FillList<T: class>(ACollection: TObject;
      ACriteria: ICriteria = nil); overload;
    function ListAll<T: class>():

{$IF CompilerVersion > 22}TObjectList<T>{$ELSE}TdormObjectList<T>{$IFEND};
      deprecated;
    // transaction
    procedure StartTransaction;
    procedure Commit(RestartAfterCommit: boolean = false);
    procedure Rollback;
    function IsInTransaction: boolean;
    // expose mapping
    function GetMapping: ICacheMappingStrategy;
    function GetEntitiesNames: TList<String>;
  end;

implementation

uses
  SysUtils,
  dorm.Adapters,
  dorm.Utils;
{ TSession }

procedure TSession.AddAsLoadedObject(AObject: TObject;
  AMappingField: TMappingField);
begin
  LoadedObjects.Add(GetLoadedObjectHashCode(AObject, AMappingField), AObject);
end;

procedure TSession.AddAsLoadedObject(ACollection: IWrappedList;
  AMappingField: TMappingField);
var
  Obj: TObject;
begin
  for Obj in ACollection do
    AddAsLoadedObject(Obj, AMappingField);
end;

procedure TSession.BuildDatabase;
begin
  FPersistStrategy.GetDatabaseBuilder(GetEntitiesNames, GetMapping).Execute;
end;

procedure TSession.ClearOID(AObject: TObject);
var
  rt: TRttiType;
begin
  rt := FCTX.GetType(AObject.ClassType);
  with FMappingStrategy.GetMapping(rt) do
    TdormUtils.SetField(AObject, Id.Name, FIdNullValue);
  SetObjectStatus(AObject, osDirty, false);
end;

function TSession.Clone<T>(Obj: T): T;
begin
  Result := T(TdormUtils.Clone(Obj));
end;

procedure TSession.Commit(RestartAfterCommit: boolean);
begin
  GetStrategy.Commit;
  GetLogger.ExitLevel('TSession.Commit');
  if RestartAfterCommit then
    StartTransaction;
end;

procedure TSession.Configure(APersistenceConfiguration: TTextReader;
  AMappingConfiguration: TTextReader = nil;
  AOwnPersistenceConfigurationReader: boolean = true;
  AOwnMappingConfigurationReader: boolean = true);
var
  _ConfigText: string;
  _MappingText: string;
  _MappingStrategy: IMappingStrategy;
  _JSonConfigEnv: ISuperObject;
begin
  try
    _ConfigText := APersistenceConfiguration.ReadToEnd;
  finally
    if AOwnPersistenceConfigurationReader then
      APersistenceConfiguration.Free;
  end;
  try
    FConfig := TSuperObject.ParseString(PWideChar(_ConfigText), true);
    if not assigned(FConfig) then
      raise Exception.Create('Cannot parse persistence configuration');
    _JSonConfigEnv := FConfig.O['persistence'].O[GetEnv];
    FConfig.O['mapping'] := nil; // is needed to avoid embedded configuration
    // Check for the old persistence file version
    _MappingText := '';
    if assigned(FConfig.O['config']) then
      _MappingText := FConfig.O['config'].s['mapping_file'];
    if _MappingText <> '' then
      raise EdormException.Create
        ('WARNING! Is no more allowed to specify the mapping file inside the persistence file');
    if AMappingConfiguration <> nil then
    begin
      try
        _MappingText := AMappingConfiguration.ReadToEnd;
      finally
        if AOwnMappingConfigurationReader then
          AMappingConfiguration.Free;
      end;
      FConfig.O['mapping'] := TSuperObject.ParseString
        (PWideChar(_MappingText), true);
      if FConfig.O['mapping'] = nil then
        raise Exception.Create('Cannot parse mapping configuration');
    end;
    ReadIDConfig(_JSonConfigEnv);
    FMappingStrategy := TCacheMappingStrategy.Create;
    if AMappingConfiguration <> nil then
    begin
      _MappingStrategy := TFileMappingStrategy.Create(FConfig.O['mapping']);
      FMappingStrategy.Add(_MappingStrategy);
    end;
    _MappingStrategy := TAttributesMappingStrategy.Create;
    FMappingStrategy.Add(_MappingStrategy);
    _MappingStrategy := TCoCMappingStrategy.Create;
    FMappingStrategy.Add(_MappingStrategy);
    FLogger := CreateLogger;
    FPersistStrategy := GetStrategy;
    FPersistStrategy.ConfigureStrategy(_JSonConfigEnv);
    FPersistStrategy.InitStrategy;
  except
    on E: Exception do
    begin
      try
        if GetLogger <> nil then
          GetLogger.Error(E.Message);
      except
      end;
      raise;
    end;
  end;

  // DuckSupport
  FValidatingDuck := TDuckTypedObject.Create;

end;

procedure TSession.ReadIDConfig(const AJsonPersistenceConfig: ISuperObject);
var
  _KeyType: string;
begin
  _KeyType := AJsonPersistenceConfig.s['key_type'];
  if (SameText(_KeyType, 'integer')) then
  begin
    FIdType := ktInteger;
    FIdNullValue := AJsonPersistenceConfig.I['null_key_value'];
  end
  else if (SameText(_KeyType, 'string')) then
  begin
    FIdType := ktString;
    FIdNullValue := AJsonPersistenceConfig.s['null_key_value'];
  end
  else
  begin
    if (SameText(_KeyType, 'integer')) then
    begin
      FIdType := ktInteger;
      FIdNullValue := AJsonPersistenceConfig.I['null_key_value'];
    end
    else if (SameText(_KeyType, 'string')) then
    begin
      FIdType := ktString;
      FIdNullValue := AJsonPersistenceConfig.s['null_key_value'];
    end
    else
      raise EdormException.Create
        ('Undefined configurations for IdType and IDNullValue');
  end;
end;

procedure TSession.CopyObject(SourceObject, TargetObject: TObject);
begin
  TdormUtils.CopyObject(SourceObject, TargetObject);
end;

function TSession.Count(AClassType: TClass; ACriteria: ICriteria): Int64;
var
  TableMapping: TMappingTable;
begin
  TableMapping := FMappingStrategy.GetMapping(FCTX.GetType(AClassType));
  if ACriteria = nil then
  begin
    Result := GetStrategy.Count(TableMapping);
  end
  else
  begin
    Result := GetStrategy.ExecuteAndGetFirst(GetStrategy.GetCountSQL(ACriteria,
      TableMapping));
  end;
end;

constructor TSession.CreateSession(Environment: TdormEnvironment);
begin
  inherited Create;
  FEnvironment := Environment;
  CurrentObjectStatus := osUnknown;
  SetLength(EnvironmentNames, 3);
  EnvironmentNames[ord(deDevelopment)] := 'development';
  EnvironmentNames[ord(deTest)] := 'test';
  EnvironmentNames[ord(deRelease)] := 'release';
  FLoadEnterExitCounter := 0;
  LoadedObjects := TObjectDictionary<string, TObject>.Create;
end;

class function TSession.CreateConfigured(APersistenceConfiguration: TTextReader;
  AMappingConfiguration: TTextReader; AEnvironment: TdormEnvironment): TSession;
begin
  Result := TSession.CreateSession(AEnvironment);
  try
    Result.Configure(APersistenceConfiguration, AMappingConfiguration);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TSession.CreateConfigured(APersistenceConfiguration: TTextReader;
  AEnvironment: TdormEnvironment): TSession;
begin
  Result := CreateConfigured(APersistenceConfiguration, nil, AEnvironment);
end;

class function TSession.CreateConfigured(APersistenceConfigurationFile: String;
  AEnvironment: TdormEnvironment): TSession;
begin
  Result := CreateConfigured(TFile.OpenText(APersistenceConfigurationFile),
    AEnvironment);
end;

function TSession.CreateLogger: IdormLogger;
var
  LogClassName: string;
  l: TRttiType;
  Section: ISuperObject;
begin
  // section config/logger_class_name is deprecated
  // if there is a specific logger_class_name per the devenv, so we'll use that
  LogClassName := '';
  Section := FConfig.O['persistence'].O[GetEnv];
  LogClassName := Section.s['logger_class_name'];
  if LogClassName = EmptyStr then
    LogClassName := FConfig.O['config'].s['logger_class_name']; // fallback
  if LogClassName = EmptyStr then
    Result := nil // consider null object pattern
  else
  begin
    l := FCTX.FindType(LogClassName);
    if not assigned(l) then
      raise EdormException.CreateFmt('Cannot find logger [%s]', [LogClassName]);
    if Supports(l.AsInstance.MetaclassType, IdormLogger) then
      Supports(l.AsInstance.MetaclassType.Create, IdormLogger, Result);
  end;
end;

procedure TSession.Delete(AObject: TObject);
var
  _v: TdormValidateable;
begin
  if IsObjectStatusAvailable(AObject) then
    raise EdormException.Create
      ('Delete cannot be called if [ObjStatus] is enabled');
  _v := WrapAsValidateableObject(AObject);
  try
    InternalDelete(AObject, _v);
  finally
    _v.Free;
  end;
end;

procedure TSession.DeleteAll(AClassType: TClass);
var
  _Type: TRttiType;
begin
  _Type := FCTX.GetType(AClassType);
  GetStrategy.DeleteAll(FMappingStrategy.GetMapping(_Type));
end;

procedure TSession.DeleteAndFree(AObject: TObject);
var
  _v: TdormValidateable;
begin
  if IsObjectStatusAvailable(AObject) then
    raise EdormException.Create
      ('DeleteAndFree cannot be called if [ObjStatus] is enabled');

  _v := WrapAsValidateableObject(AObject);
  try
    InternalDelete(AObject, _v);
  finally
    _v.Free;
  end;

  FreeAndNil(AObject);
end;

procedure TSession.DeleteHasManyRelation(AMappingTable: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
var
  _has_many: TMappingRelation;
  _child_type: TRttiType;
  v: TValue;
  List: IWrappedList;
  Obj: TObject;
begin
  if CurrentObjectStatus <> osUnknown then
    raise EdormException.Create
      ('This method should not be called in a ObjectStatus <> osUnknown');
  GetLogger.EnterLevel('has_many ' + ARttiType.ToString);
  GetLogger.Debug('Deleting has_many for ' + ARttiType.ToString);
  for _has_many in AMappingTable.HasManyList do
  begin
    v := TdormUtils.GetField(AObject, _has_many.Name);
    _child_type := FCTX.FindType(Qualified(AMappingTable,
      _has_many.ChildClassName));
    if not assigned(_child_type) then
    begin
      raise Exception.Create('Unknown type ' + _has_many.ChildClassName);
    end;
    List := WrapAsList(v.AsObject);
    // if the relation is LazyLoad...
    { todo: The has_many rows should be deleted also if they are lazy_loaded }
    { todo: optimize the delete? }
    if assigned(List) then
    begin
      for Obj in List do
        Delete(Obj);
    end;
  end;
  GetLogger.ExitLevel('has_many ' + ARttiType.ToString);
end;

procedure TSession.DeleteHasOneRelation(AMappingTable: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
var
  _has_one: TMappingRelation;
  _child_type: TRttiType;
  v: TValue;
  Obj: TObject;
begin
  GetLogger.EnterLevel('has_one ' + ARttiType.ToString);
  GetLogger.Debug('Deleting has_one for ' + ARttiType.ToString);
  for _has_one in AMappingTable.HasOneList do
  begin
    v := TdormUtils.GetField(AObject, _has_one.Name);
    _child_type := FCTX.FindType(Qualified(AMappingTable,
      _has_one.ChildClassName));
    if not assigned(_child_type) then
      raise Exception.Create('Unknown type ' + _has_one.ChildClassName);
    Obj := v.AsObject; // if the relation is LazyLoad...
    { todo: has_one row should be deleted also if they are lazy_loaded }
    if assigned(Obj) then
      Delete(Obj);
  end;
  GetLogger.ExitLevel('has_one ' + ARttiType.ToString);
end;

destructor TSession.Destroy;
begin
  LoadedObjects.Free;
  if assigned(FPersistStrategy) then
  begin
    if FPersistStrategy.InTransaction then
      Commit; // Default action "COMMIT"
    FPersistStrategy := nil;
  end;
  if assigned(FLogger) then
    FLogger := nil;
  FValidatingDuck.Free;
  FValidatingDuck := nil;
  inherited;
end;

procedure TSession.DisableLazyLoad(AClass: TClass; const APropertyName: string);
begin
  SetLazyLoadFor(AClass.ClassInfo, APropertyName, false);
end;

procedure TSession.EnableLazyLoad(AClass: TClass; const APropertyName: string);
begin
  SetLazyLoadFor(AClass.ClassInfo, APropertyName, true);
end;

// QUESTA RIMANE
procedure TSession.FillList<T>(ACollection: TObject; ACriteria: ICriteria);
var
  rt: TRttiType;
  _table: TMappingTable;
  _fields: TMappingFieldList;
  _type_info: PTypeInfo;
  SearcherClassname: string;
  List: IWrappedList;
  Obj: TObject;
  _validateable: TdormValidateable;
begin
  _type_info := TypeInfo(T);
  rt := FCTX.GetType(_type_info);
  _table := FMappingStrategy.GetMapping(rt);
  _fields := _table.Fields;
  if assigned(ACriteria) then
    SearcherClassname := TObject(ACriteria).ClassName
  else
    SearcherClassname := 'Reading all rows from ' + _table.TableName;
  GetLogger.EnterLevel(SearcherClassname);
  TdormUtils.MethodCall(ACollection, 'Clear', []);
  GetStrategy.LoadList(ACollection, rt, _table, ACriteria);
  List := WrapAsList(ACollection);
  for Obj in List do
  begin
    SetObjectStatus(Obj, osClean, false);
    _validateable := WrapAsValidateableObject(Obj, FValidatingDuck);
    _validateable.OnAfterLoad;
  end;
  GetLogger.ExitLevel(SearcherClassname);
end;

function TSession.GetLoadedObjectHashCode(AObject: TObject;
  AMappingField: TMappingField): String;
begin
  Result := AObject.ClassName + '_' +
    inttostr(GetIdValue(AMappingField, AObject).AsInt64);
end;

function TSession.GetLoadedObjectHashCode(ATypeInfo: PTypeInfo;
  AValue: TValue): String;
begin
  Result := ATypeInfo.Name + '_' + inttostr(AValue.AsInt64);
end;

function TSession.GetLogger: IdormLogger;
begin
  Result := FLogger;
end;

function TSession.GetMapping: ICacheMappingStrategy;
begin
  Result := FMappingStrategy;
end;

function TSession.GetObjectStatus(AObject: TObject): TdormObjectStatus;
var
  _Type: TRttiType;
  _objstatus: TRttiProperty;
begin
  _Type := FCTX.GetType(AObject.ClassInfo);
  _objstatus := _Type.GetProperty('ObjStatus');
  if not assigned(_objstatus) then
    Exit(osUnknown);
  Result := TdormObjectStatus(_objstatus.GetValue(AObject).AsOrdinal);
end;

function TSession.GetPackageName(AMappingTable: TMappingTable;
  const AClassName: string): string;
begin
  Result := AMappingTable.Package;
end;

function TSession.GetStrategy: IdormPersistStrategy;
var
  T: TRttiType;
  AdapterClassName: SOString;
begin
  if not assigned(FPersistStrategy) then
  begin
    AdapterClassName := FConfig.O['persistence'].O[GetEnv].s
      ['database_adapter'];
    T := FCTX.FindType(AdapterClassName);
    if assigned(T) then
    begin
      if Supports(T.AsInstance.MetaclassType, IdormPersistStrategy) then
      begin
        Supports(T.AsInstance.MetaclassType.Create,
          IdormPersistStrategy, Result);
        FPersistStrategy := Result;
        FPersistStrategy.SetLogger(FLogger);
      end
      else
        raise Exception.CreateFmt
          ('Adapter [%s] does not support IdormPersistStrategy',
          [AdapterClassName]);
    end
    else
      raise Exception.CreateFmt
        ('Adapter [%s] not found. Have you enabled it in dorm.inc?',
        [AdapterClassName]);
  end
  else
    Result := FPersistStrategy;
end;

procedure TSession.HandleDirtyPersist(AIdValue: TValue; AObject: TObject;
  AValidateable: TdormValidateable);
begin
  case FIdType of
    ktInteger:
      begin
        if AIdValue.AsInteger = FIdNullValue.AsInteger then
          InternalInsert(AObject, AValidateable)
        else
          InternalUpdate(AObject, AValidateable);
      end;
    ktString:
      begin
        if AIdValue.AsString = FIdNullValue.AsString then
          InternalInsert(AObject, AValidateable)
        else
          InternalUpdate(AObject, AValidateable);
      end;
  end;
end;
{ *** not used
  function TSession.GetPersistentClassesName(WithPackage: Boolean): TList<string>;
  var
  A: TSuperArray;
  i: Integer;
  begin
  Result := TList<string>.Create;
  A := FConfig.O['persistence'].A['persistent_classes'];
  if A.Length = 0 then
  EdormException.Create('persistent_classes non present or not valid');
  for i := 0 to A.Length - 1 do
  if WithPackage then
  Result.Add(FConfig.O['mapping'].O[A.s[i]].s['package'] + '.' + A.s[i])
  else
  Result.Add(A.s[i]);
  end;
}

function TSession.GetIdValue(AIdMappingField: TMappingField;
  AObject: TObject): TValue;
begin
  Assert(AIdMappingField <> nil);
  Result := TdormUtils.GetField(AObject, AIdMappingField.Name);
end;

function TSession.GetEntitiesNames: TList<String>;
var
  alltypes: TArray<TRttiType>;
  _Type: TRttiType;
  _attributes: TArray<TCustomAttribute>;
  _attrib: TCustomAttribute;
begin
  Result := TList<String>.Create;
  alltypes := FCTX.GetTypes;
  for _Type in alltypes do
  begin
    _attributes := _Type.GetAttributes;
    for _attrib in _attributes do
      if _attrib is Entity then
      begin

{$IF CompilerVersion > 22}

        Result.Add(_Type.QualifiedClassName);

{$ELSE}

        Result.Add(_Type.QualifiedName);

{$IFEND}

        Break;
      end;
  end;
end;

function TSession.GetEnv: string;
begin
  Result := EnvironmentNames[ord(FEnvironment)];
end;

function TSession.LoadByMappingField(ATypeInfo: PTypeInfo;
  AMappingField: TMappingField; const Value: TValue): TObject;
var
  rt: TRttiType;
  _table: TMappingTable;
  Obj: TObject;
  _validateable: TdormValidateable;
  _validable: TdormValidateable;
begin
  Obj := nil;
  LoadEnter;
  rt := FCTX.GetType(ATypeInfo);
  GetLogger.EnterLevel('Load ' + rt.ToString);
  _table := FMappingStrategy.GetMapping(rt);
  if not IsAlreadyLoaded(ATypeInfo, Value, Result)
  then { todo: optimize... please }
  begin
    Obj := TdormUtils.CreateObject(rt);
    try
      if FPersistStrategy.Load(rt, _table, AMappingField, Value, Obj) then
      begin
        Result := Obj;
        AddAsLoadedObject(Result, _table.Id);
        // mark this object as already loaded
      end
      else
      begin
        Obj.Free;
        Result := nil;
      end;
    except
      Obj.Free;
      raise;
    end;

    if assigned(Result) then
    begin
      LoadBelongsToRelation(_table, Value, rt, Result);
      LoadHasManyRelation(_table, Value, rt, Result);
      LoadHasOneRelation(_table, Value, rt, Result);
    end;

    if assigned(Obj) then
    begin
      _validable := WrapAsValidateableObject(Obj, FValidatingDuck);
      _validable.OnAfterLoad;
    end;
  end;
  LoadExit;
  GetLogger.ExitLevel('Load ' + rt.ToString);
end;

function TSession.Load(ATypeInfo: PTypeInfo; const Value: TValue): TObject;
var
  rt: TRttiType;
  _table: TMappingTable;
  // _idValue: TValue;
  // Obj: TObject;
begin
  rt := FCTX.GetType(ATypeInfo);
  GetLogger.EnterLevel('Load ' + rt.ToString);
  _table := FMappingStrategy.GetMapping(rt);
  Result := LoadByMappingField(ATypeInfo, _table.Id, Value);
  SetObjectStatus(Result, osClean, false);
  GetLogger.ExitLevel('Load ' + rt.ToString);
end;

function TSession.Load<T>(const Value: TValue): T;
begin
  Result := T(Load(TypeInfo(T), Value));
end;

function TSession.ListAll<T>:

{$IF CompilerVersion > 22}TObjectList<T>{$ELSE}TdormObjectList<T>{$IFEND};
begin
  Result := LoadList<T>(nil);
end;

function TSession.Load(ATypeInfo: PTypeInfo; const Value: TValue;
  AObject: TObject): boolean;
var
  rt: TRttiType;
  _table: TMappingTable;
  _idValue: TValue;
  _validateable: TdormValidateable;
begin
  LoadEnter;
  rt := FCTX.GetType(ATypeInfo);
  GetLogger.EnterLevel('Load ' + rt.ToString);
  _table := FMappingStrategy.GetMapping(rt);
  _validateable := WrapAsValidateableObject(AObject, FValidatingDuck);
  _validateable.OnBeforeLoad;
  Result := FPersistStrategy.Load(rt, _table, Value, AObject);
  if Result then
  begin
    _idValue := GetIdValue(_table.Id, AObject);
    LoadBelongsToRelation(_table, _idValue, rt, AObject);
    LoadHasManyRelation(_table, _idValue, rt, AObject);
    LoadHasOneRelation(_table, _idValue, rt, AObject);
    _validateable.OnAfterLoad;
  end;

  GetLogger.ExitLevel('Load ' + rt.ToString);
  LoadExit;
end;

function TSession.Load<T>(const Value: TValue; AObject: TObject): boolean;
begin
  Result := Load(TypeInfo(T), Value, AObject);
end;

procedure TSession.LoadHasManyRelation(ATableMapping: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject;
  AConsiderLazyLoading: boolean);
var
  _has_many: TMappingRelation;
begin
  GetLogger.EnterLevel('has_many ' + ARttiType.ToString);
  for _has_many in ATableMapping.HasManyList do
  begin
    if (not AConsiderLazyLoading) or (not _has_many.LazyLoad) then
      LoadHasManyRelationByPropertyName(AIdValue, ARttiType, ARttiType.ToString,
        _has_many.Name, AObject);
  end;
  GetLogger.ExitLevel('has_many ' + ARttiType.ToString);
end;

procedure TSession.LoadHasManyRelationByPropertyName(AIdValue: TValue;
  ARttiType: TRttiType; AClassName: string; APropertyName: string;
  var AObject: TObject);
var
  _has_many: TMappingRelation;
  Table, ChildTable: TMappingTable;
  v: TValue;
  _child_type: TRttiType;
  AttributeNameInTheParentObject: string;
  Coll: TObject;
  Criteria: ICriteria;
begin
  GetLogger.Debug('Loading HAS_MANY for ' + AClassName + '.' + APropertyName);
  Table := FMappingStrategy.GetMapping(ARttiType);
  _has_many := GetMappingRelationByPropertyName(Table.HasManyList,
    APropertyName);
  if assigned(_has_many) then
  begin
    AttributeNameInTheParentObject := _has_many.Name;
    _child_type := FCTX.FindType(Qualified(Table, _has_many.ChildClassName));
    UpdateChildTypeWithRealListInnerType(Table, _child_type);
    if not assigned(_child_type) then
      raise Exception.Create('Unknown type ' + _has_many.ChildClassName);
    v := TdormUtils.GetProperty(AObject, AttributeNameInTheParentObject);
    Coll := v.AsObject;
    if assigned(Coll) then
    begin
      TdormUtils.MethodCall(Coll, 'Clear', []);
      Criteria := TdormCriteria.NewCriteria(_has_many.ChildFieldName,
        TdormCompareOperator.coEqual,
        GetIdValue(FMappingStrategy.GetMapping(ARttiType).Id, AObject));
      ChildTable := FMappingStrategy.GetMapping(_child_type);
      GetStrategy.LoadList(Coll, _child_type, ChildTable, Criteria);
      { todo: callafterloadevent }
      AddAsLoadedObject(WrapAsList(Coll), ChildTable.Id);
      // LoadList<TObject>(Criteria, Coll, [CallAfterLoadEvent]);
      LoadRelationsForEachElement(Coll);
    end;
    // else
    // raise EdormException.CreateFmt
    // ('DORM is trying to fill the HasMany list for %s but it is nil',
    // [AClassName + '.' + APropertyName]);
  end
  else
    raise Exception.Create('Unknown property name ' + APropertyName);
end;

procedure TSession.LoadHasOneRelation(ATableMapping: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject;
  AConsiderLazyLoading: boolean);
var
  _has_one: TMappingRelation;
begin
  GetLogger.EnterLevel('has_one ' + ARttiType.ToString);
  for _has_one in ATableMapping.HasOneList do
  begin
    if (not AConsiderLazyLoading) or (not _has_one.LazyLoad) then
      LoadHasOneRelationByPropertyName(AIdValue, ARttiType, ARttiType.ToString,
        _has_one.Name, AObject);
  end;
  GetLogger.ExitLevel('has_one ' + ARttiType.ToString);
end;

procedure TSession.LoadBelongsToRelation(ATableMapping: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject;
  AConsiderLazyLoading: boolean);
var
  _belongs_to: TMappingBelongsTo;
begin
  GetLogger.EnterLevel('belongs_to ' + ARttiType.ToString);
  for _belongs_to in ATableMapping.BelongsToList do
  begin
    if (not AConsiderLazyLoading) or (not _belongs_to.LazyLoad) then
      LoadBelongsToRelationByPropertyName(AIdValue, ARttiType,
        ARttiType.ToString, _belongs_to.Name, AObject);
  end;
  GetLogger.ExitLevel('belongs_to ' + ARttiType.ToString);
end;

procedure TSession.LoadHasOneRelationByPropertyName(AIdValue: TValue;
  ARttiType: TRttiType; AClassName, APropertyName: string;
  var AObject: TObject);
var
  _table: TMappingTable;
  _has_one: TMappingRelation;
  _child_type: TRttiType;
  DestObj, Obj: TObject;
  _validable: TdormValidateable;
begin
  GetLogger.Debug('Loading HAS_ONE for ' + AClassName + '.' + APropertyName);
  _table := FMappingStrategy.GetMapping(ARttiType);
  _has_one := GetMappingRelationByPropertyName(_table.HasOneList,
    APropertyName);
  if assigned(_has_one) then
  begin
    if not _has_one.LazyLoad then
    begin
      _child_type := FCTX.FindType(Qualified(_table, _has_one.ChildClassName));
      if not assigned(_child_type) then
        raise Exception.Create('Unknown type ' + _has_one.ChildClassName);
      if _has_one.ChildFieldName = EmptyStr then
        raise Exception.Create('Empty child_field_name for ' +
          _has_one.ChildClassName);
      DestObj := TdormUtils.GetField(AObject, _has_one.Name).AsObject;
      // If the HasOne reference is not created, then Create It!
      if not assigned(DestObj) then
      begin
        if IsAlreadyLoaded(_child_type.Handle, AIdValue, Obj) then
          DestObj := Obj
        else
          DestObj := LoadByMappingField(_child_type.Handle,
            FMappingStrategy.GetMapping(_child_type)
            .FindByName(_has_one.ChildFieldName), AIdValue);
        TdormUtils.SetProperty(AObject, _has_one.Name, DestObj);
      end
      else
      begin
        if IsAlreadyLoaded(_child_type.Handle, AIdValue, Obj) then
        begin
          DestObj.Free;
          DestObj := Obj;
        end
        else
        begin
          LoadByMappingField(_child_type.Handle,
            FMappingStrategy.GetMapping(_child_type)
            .FindByName(_has_one.ChildFieldName), AIdValue, DestObj);
        end;
      end;
      SetObjectStatus(DestObj, osClean, false);
    end;
  end
  else
    raise Exception.Create('Unknown property name ' + APropertyName);
end;

function TSession.LoadList<T>(Criteria: ICriteria):

{$IF CompilerVersion > 22}TObjectList<T>{$ELSE}TdormObjectList<T>{$IFEND};
begin
  Result := {$IF CompilerVersion > 22}TObjectList<T>{$ELSE}TdormObjectList<T>{$IFEND}.Create(true);
  LoadList<T>(Criteria, Result);
end;

procedure TSession.LoadList<T>(Criteria: ICriteria; AObject: TObject);
var
  SQL: string;
  Table: TMappingTable;
  ItemClassInfo: PTypeInfo;
  rt: TRttiType;
  _fields: TMappingFieldList;
  SearcherClassname: string;
  List: IWrappedList;
  Obj: TObject;
  CustomCrit: ICustomCriteria;
  _validateable: TdormValidateable;
begin
  ItemClassInfo := TypeInfo(T);
  rt := FCTX.GetType(ItemClassInfo);
  Table := FMappingStrategy.GetMapping(rt);
  if Supports(Criteria, ICustomCriteria, CustomCrit) then
  begin
    SQL := CustomCrit.GetSQL
  end
  else
    SQL := GetStrategy.GetSelectSQL(Criteria, Table);
  if assigned(Criteria) then
    SearcherClassname := TObject(Criteria).ClassName
  else
    SearcherClassname :=
      '<Criteria = nil, Full select automatically generated>';
  GetLogger.EnterLevel(SearcherClassname);
  TdormUtils.MethodCall(AObject, 'Clear', []);

  { TODO -oDaniele -cLoadList : Event "OnBeforeLoad" cannot be called here. Changes to the strategies is required }
  GetStrategy.LoadList(AObject, rt, Table, Criteria);
  List := WrapAsList(AObject);
  for Obj in List do
  begin
    _validateable := WrapAsValidateableObject(Obj, FValidatingDuck);
    _validateable.OnAfterLoad;
  end;
  GetLogger.ExitLevel(SearcherClassname);
end;

procedure TSession.LoadRelationsForEachElement(AList: TObject;
  ARelationsSet: TdormRelations; AConsiderLazyLoading: boolean);
var
  el: TObject;
  List: IWrappedList;
begin
  List := WrapAsList(AList);
  for el in List do
    LoadRelations(el, ARelationsSet, AConsiderLazyLoading);
end;

function TSession.OIDIsSet(AObject: TObject): boolean;
var
  _table: TMappingTable;
  _idValue: TValue;
begin
  _table := FMappingStrategy.GetMapping(FCTX.GetType(AObject.ClassType));
  _idValue := GetIdValue(_table.Id, AObject);
  Result := not IsNullKey(_table, _idValue);
end;

procedure TSession.Persist(AObject: TObject);
var
  _idValue: TValue;
  _table: TMappingTable;
  _Type: TRttiType;
  _v: TdormValidateable;
begin
  _Type := FCTX.GetType(AObject.ClassInfo);
  _table := FMappingStrategy.GetMapping(_Type);
  if not assigned(_table.Id) then
    raise EdormException.CreateFmt('Not available OID for %s', [_Type.Name]);
  _idValue := GetIdValue(_table.Id, AObject);
  if IsObjectStatusAvailable(AObject) then
  begin
    _v := WrapAsValidateableObject(AObject);
    try
      CurrentObjectStatus := self.GetObjectStatus(AObject);
      case CurrentObjectStatus of
        osDirty:
          begin
            _v.OnBeforePersist;
            HandleDirtyPersist(_idValue, AObject, _v);
            _v.OnAfterPersist;
          end;
        osClean: // update the only the child
          begin
            _v.OnBeforePersist;
            InternalUpdate(AObject, _v, true);
            _v.OnAfterPersist;
          end;
        osUnknown:
          begin
            raise EdormException.Create
              ('Cannot call Persist is objects do not supports ObjStatus');
          end;
        osDeleted:
          begin
            _v.OnBeforePersist;
            InternalDelete(AObject, _v);
            _v.OnAfterPersist;
          end;
      end;
    finally
      _v.Free;
    end;
  end
  else
  begin
    case FIdType of
      ktInteger:
        begin
          if _idValue.AsInteger = FIdNullValue.AsInteger then
            Insert(AObject)
          else
            Update(AObject);
        end;
      ktString:
        begin
          if _idValue.AsString = FIdNullValue.AsString then
            Insert(AObject)
          else
            Update(AObject);
        end;
    end;
  end;
end;

procedure TSession.PersistCollection(ACollection: IWrappedList);
var
  Obj: TObject;
begin
  for Obj in ACollection do
  begin
    Persist(Obj);
  end;
end;

procedure TSession.PersistCollection(ACollection: TObject);
var
  List: IWrappedList;
begin
  List := WrapAsList(ACollection);
  PersistCollection(List);
end;

procedure TSession.PersistHasManyRelation(AMappingTable: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
var
  _has_many: TMappingRelation;
  _child_type: TRttiType;
  v: TValue;
  List: IWrappedList;
  Obj: TObject;
  _table: TMappingTable;
  _idValue: TValue;
  _type_name: string;
  _p1, _p2: Integer;
  attr: ListOf;
begin
  GetLogger.EnterLevel('persist has_many ' + ARttiType.ToString);
  for _has_many in AMappingTable.HasManyList do
  begin
    v := TdormUtils.GetField(AObject, _has_many.Name);
    GetLogger.Debug('-- Inspecting for ' + _has_many.ChildClassName);
    _type_name := Qualified(AMappingTable, _has_many.ChildClassName);
    _child_type := FCTX.FindType(_type_name);
    if not assigned(_child_type) then
      raise Exception.Create('Unknown type ' + _has_many.ChildClassName);
    UpdateChildTypeWithRealListInnerType(AMappingTable, _child_type);
    _table := FMappingStrategy.GetMapping(_child_type);
    Obj := v.AsObject;
    if assigned(Obj) then
    begin
      List := WrapAsList(Obj);
      for Obj in List do
      begin
        GetLogger.Debug('-- Saving ' + _child_type.QualifiedName);
        GetLogger.Debug('----> Setting property ' + _has_many.ChildFieldName);
        TdormUtils.SetField(Obj, _has_many.ChildFieldName, AIdValue);
        Persist(Obj); // help me!!!
        {
          case GetObjectStatus(Obj) of
          osDirty:
          begin
          _idValue := GetIdValue(_table.Id, Obj);
          HandleDirtyPersist(_idValue, Obj);
          SetObjectStatus(Obj, osClean);
          end;
          osClean:
          begin
          InternalUpdate(Obj, true);
          end;
          osUnknown:
          begin
          raise EdormException.CreateFmt
          ('[%s] doesn''t support ObjStatus. You should not call Persist* methods if not all the objects supports ObjStatus',
          [Obj.ClassName]);
          end;
          osDeleted:
          begin
          InternalDelete(Obj);
          end;
          end;
        }
      end;
    end;
  end;
  GetLogger.ExitLevel('persist has_many ' + ARttiType.ToString);
end;

procedure TSession.PersistHasOneRelation(AMappingTable: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
var
  _has_one: TMappingRelation;
  _child_type: TRttiType;
  v: TValue;
  Obj: TObject;
  _child_mapping: TMappingTable;
  _CurrentChildIDValue: TValue;
begin
  GetLogger.EnterLevel('persist has_one ' + ARttiType.ToString);
  // GetLogger.Debug('Saving _has_one for ' + ARttiType.ToString);
  for _has_one in AMappingTable.HasOneList do
  begin
    v := TdormUtils.GetField(AObject, _has_one.Name);
    GetLogger.Debug('-- Inspecting for ' + _has_one.ChildClassName);
    _child_type := FCTX.FindType(Qualified(AMappingTable,
      v.AsObject.ClassName));
    if not assigned(_child_type) then
      raise Exception.Create('Unknown type ' + _has_one.ChildClassName);
    Obj := v.AsObject;
    if assigned(Obj) then
    begin
      if GetObjectStatus(Obj) = osUnknown then
        raise EdormException.CreateFmt
          ('[%s] doesn''t support ObjStatus. You should not call Persist* methods if not all the objects supports ObjStatus',
          [Obj.ClassName]);
      GetLogger.Debug('-- Saving ' + _child_type.QualifiedName);
      GetLogger.Debug('----> Setting property ' + _has_one.ChildFieldName);
      TdormUtils.SetField(Obj, _has_one.ChildFieldName, AIdValue);
      _child_mapping := FMappingStrategy.GetMapping(_child_type);
      _CurrentChildIDValue := GetIdValue(_child_mapping.Id, Obj);
      Persist(Obj);
      // HandleDirtyPersist(_CurrentChildIDValue, Obj);
    end;
  end;
  GetLogger.ExitLevel('persist has_one ' + ARttiType.ToString);
end;

procedure TSession.LoadRelations(AObject: TObject;
  ARelationsSet: TdormRelations; AConsiderLazyLoading: boolean);
var
  rt: TRttiType;
  _table: TMappingTable;
  _idValue: TValue;
begin
  if assigned(AObject) then
  begin
    rt := FCTX.GetType(AObject.ClassType);
    _table := FMappingStrategy.GetMapping(rt);
    _idValue := GetIdValue(FMappingStrategy.GetMapping(rt).Id, AObject);
    if [drAll, drBelongsTo] * ARelationsSet <> [] then
      LoadBelongsToRelation(_table, _idValue, rt, AObject,
        AConsiderLazyLoading);
    if [drAll, drHasMany] * ARelationsSet <> [] then
      LoadHasManyRelation(_table, _idValue, rt, AObject, AConsiderLazyLoading);
    if [drAll, drHasOne] * ARelationsSet <> [] then
      LoadHasOneRelation(_table, _idValue, rt, AObject, AConsiderLazyLoading);
  end;
end;

procedure TSession.LoadBelongsToRelationByPropertyName(AIdValue: TValue;
  ARttiType: TRttiType; AClassName, APropertyName: string;
  var AObject: TObject);
var
  _table: TMappingTable;
  _belongs_to: TMappingBelongsTo;
  _belong_type: TRttiType;
  _belong_field_key_value: TValue;
  v: TValue;
  parent_mapping: TMappingTable;
begin
  GetLogger.Debug('Loading BELONGS_TO for ' + AClassName + '.' + APropertyName);
  _table := FMappingStrategy.GetMapping(ARttiType);
  _belongs_to := GetMappingBelongsToByPropertyName(_table.BelongsToList,
    APropertyName);
  if assigned(_belongs_to) then
  begin
    _belong_type := FCTX.FindType(Qualified(_table,
      _belongs_to.OwnerClassName));
    if not assigned(_belong_type) then
      raise Exception.Create('Unknown type ' + _belongs_to.OwnerClassName);
    _belong_field_key_value := TdormUtils.GetProperty(AObject,
      _belongs_to.RefFieldName);
    parent_mapping := FMappingStrategy.GetMapping
      (FCTX.FindType(Qualified(_table, _belongs_to.OwnerClassName)));
    // parent_field_mapping := child_mapping.Id;
    v := LoadByMappingField(FCTX.FindType(Qualified(_table,
      _belongs_to.OwnerClassName)).Handle, parent_mapping.Id,
      _belong_field_key_value);
    //
    // _belong_field_key_value);
    TdormUtils.SetField(AObject, _belongs_to.Name, v);
    SetObjectStatus(v.AsObject, osClean, false);
  end
  else
    raise Exception.Create('Unknown property name ' + APropertyName);
end;

function TSession.LoadByMappingField(ATypeInfo: PTypeInfo;
  AMappingField: TMappingField; const Value: TValue; AObject: TObject): boolean;
var
  rt: TRttiType;
  _table: TMappingTable;
  _idValue: TValue;
  _validateable: TdormValidateable;
begin
  LoadEnter;
  rt := FCTX.GetType(ATypeInfo);
  GetLogger.EnterLevel('Load ' + rt.ToString);
  _table := FMappingStrategy.GetMapping(rt);
  _validateable := WrapAsValidateableObject(AObject, FValidatingDuck);
  _validateable.OnBeforeLoad;
  Result := FPersistStrategy.Load(rt, _table, AMappingField, Value, AObject);
  if Result then
  begin
    _idValue := GetIdValue(_table.Id, AObject);
    LoadBelongsToRelation(_table, _idValue, rt, AObject);
    LoadHasManyRelation(_table, _idValue, rt, AObject);
    LoadHasOneRelation(_table, _idValue, rt, AObject);
    _validateable.OnAfterLoad;
  end;
  GetLogger.ExitLevel('Load ' + rt.ToString);
  LoadExit;
end;

procedure TSession.LoadEnter;
begin
  Inc(FLoadEnterExitCounter);
end;

procedure TSession.LoadExit;
begin
  Dec(FLoadEnterExitCounter);
  if FLoadEnterExitCounter = 0 then
    LoadedObjects.Clear;
end;

function TSession.Qualified(AMappingTable: TMappingTable;
  const AClassName: string): string;
var
  _p1: Integer;
  _p2: Integer;
begin
  // if AClassName = 'TRates' then
  // Exit('ModelQuaestioU.TRate');
  Result := GetPackageName(AMappingTable, AClassName) + '.' + AClassName;
  // if Pos('<', Result) > 0 then // is a genric type
  // begin
  // _p1 := Pos('<', Result) + 1;
  // _p2 := Pos('>', Result);
  // Result := Copy(Result, _p1, _p2 - _p1);
  // end;
end;

procedure TSession.Rollback;
begin
  GetStrategy.Rollback;
  GetLogger.ExitLevel('TSession.Rollback');
end;

function TSession.Insert(AObject: TObject): TValue;
var
  _v: TdormValidateable;
begin
  if IsObjectStatusAvailable(AObject) then
    raise EdormException.Create
      ('Insert cannot be called if [ObjStatus] is enabled');

  _v := WrapAsValidateableObject(AObject);
  try
    Result := InternalInsert(AObject, _v);
  finally
    _v.Free;
  end;
end;

procedure TSession.Save(dormUOW: TdormUOW);
var
  c: TObjectList<TObject>;
begin
  c := dormUOW.GetUOWInsert;
  InsertCollection(c);
  c := dormUOW.GetUOWUpdate;
  UpdateCollection(c);
  c := dormUOW.GetUOWDelete;
  DeleteCollection(c);
end;

function TSession.Save(AObject: TObject): TValue;
begin
  Result := Insert(AObject);
end;

function TSession.InsertAndFree(AObject: TObject): TValue;
begin
  if IsObjectStatusAvailable(AObject) then
    raise EdormException.Create
      ('InsertAndFree cannot be called if [ObjStatus] is enabled');
  Result := InternalInsert(AObject, nil);
  FreeAndNil(AObject);
end;

procedure TSession.SetLazyLoadFor(ATypeInfo: PTypeInfo;
  const APropertyName: string; const Value: boolean);
var
  _rttitype: TRttiType;
  _has_many, _has_one: TMappingRelation;
  _belongs_to: TMappingBelongsTo;
  _table: TMappingTable;
begin
  _rttitype := FCTX.GetType(ATypeInfo);
  _table := FMappingStrategy.GetMapping(_rttitype);
  for _has_many in _table.HasManyList do
  begin
    if CompareText(_has_many.Name, APropertyName) = 0 then
    begin
      _has_many.LazyLoad := Value;
      Break;
    end;
  end;
  for _belongs_to in _table.BelongsToList do
  begin
    if CompareText(_belongs_to.Name, APropertyName) = 0 then
    begin
      _belongs_to.LazyLoad := Value;
      Break;
    end;
  end;
  for _has_one in _table.HasOneList do
  begin
    if CompareText(_has_one.Name, APropertyName) = 0 then
    begin
      _has_one.LazyLoad := Value;
      Break;
    end;
  end;
end;

procedure TSession.SetObjectStatus(AObject: TObject; AStatus: TdormObjectStatus;
  ARaiseExceptionIfNotExists: boolean);
var
  _Type: TRttiType;
  _objstatus: TRttiProperty;
begin
  if assigned(AObject) then
  begin
    _Type := FCTX.GetType(AObject.ClassInfo);
    _objstatus := _Type.GetProperty('ObjStatus');
    if (not assigned(_objstatus)) then
    begin
      if ARaiseExceptionIfNotExists then
        raise EdormException.Create('Cannot find [ObjStatus] property');
    end
    else
      _objstatus.SetValue(AObject, TValue.From<TdormObjectStatus>(AStatus));
  end;
end;

procedure TSession.InsertCollection(ACollection: TObject);
var
  Obj: TObject;
  Coll: IWrappedList;
begin
  Coll := WrapAsList(ACollection);
  for Obj in Coll do
  begin
    Insert(Obj);
  end;
end;

procedure TSession.InsertHasManyRelation(AMappingTable: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
var
  _has_many: TMappingRelation;
  _child_type: TRttiType;
  v: TValue;
  List: IWrappedList;
  Obj: TObject;
begin
  if CurrentObjectStatus <> osUnknown then
    raise EdormException.Create
      ('This method should not be called in a ObjectStatus <> osUnknown');
  GetLogger.EnterLevel('has_many ' + ARttiType.ToString);
  GetLogger.Debug('Saving has_many for ' + ARttiType.ToString);
  for _has_many in AMappingTable.HasManyList do
  begin
    v := TdormUtils.GetField(AObject, _has_many.Name);
    GetLogger.Debug('-- Inspecting for ' + _has_many.ChildClassName);
    _child_type := FCTX.FindType(Qualified(AMappingTable,
      _has_many.ChildClassName));
    if not assigned(_child_type) then
      raise Exception.Create('Unknown type ' + _has_many.ChildClassName);
    List := WrapAsList(v.AsObject);
    if assigned(List) then
    begin
      for Obj in List do
      begin
        GetLogger.Debug('-- Saving ' + _child_type.QualifiedName);
        GetLogger.Debug('----> Setting property ' + _has_many.ChildFieldName);
        TdormUtils.SetField(Obj, _has_many.ChildFieldName, AIdValue);
        Insert(Obj);
      end;
    end;
  end;
  GetLogger.ExitLevel('has_many ' + ARttiType.ToString);
end;

procedure TSession.InsertHasOneRelation(AMappingTable: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
var
  _has_one: TMappingRelation;
  _child_type: TRttiType;
  v: TValue;
  Obj: TObject;
begin
  GetLogger.EnterLevel('has_one ' + ARttiType.ToString);
  GetLogger.Debug('Saving _has_one for ' + ARttiType.ToString);
  for _has_one in AMappingTable.HasOneList do
  begin
    v := TdormUtils.GetField(AObject, _has_one.Name);
    GetLogger.Debug('-- Inspecting for ' + _has_one.ChildClassName);
    _child_type := FCTX.FindType(Qualified(AMappingTable,
      _has_one.ChildClassName));
    if not assigned(_child_type) then
      raise Exception.Create('Unknown type ' + _has_one.ChildClassName);
    Obj := v.AsObject;
    if assigned(Obj) then
    begin
      GetLogger.Debug('-- Saving ' + _child_type.QualifiedName);
      GetLogger.Debug('----> Setting property ' + _has_one.ChildFieldName);
      TdormUtils.SetField(Obj, _has_one.ChildFieldName, AIdValue);
      Insert(Obj);
    end;
  end;
  GetLogger.ExitLevel('has_one ' + ARttiType.ToString);
end;

procedure TSession.InternalDelete(AObject: TObject;
  AValidaetable: TdormValidateable);
var
  _rttitype: TRttiType;
  _table: TMappingTable;
  _class_name: string;
  _idValue: TValue;
  _validateable: TdormValidateable;
begin
  GetLogger.EnterLevel('Delete');

  _validateable := WrapAsValidateableObject(AObject, FValidatingDuck);

  _validateable.DeleteValidate;
  _validateable.OnBeforeDelete;

  _rttitype := FCTX.GetType(AObject.ClassInfo);
  _class_name := _rttitype.ToString;
  _table := FMappingStrategy.GetMapping(_rttitype);
  _idValue := GetIdValue(_table.Id, AObject);
  if CurrentObjectStatus = osUnknown then
  begin
    DeleteHasManyRelation(_table, _idValue, _rttitype, AObject);
    DeleteHasOneRelation(_table, _idValue, _rttitype, AObject);
    GetStrategy.Delete(_rttitype, AObject, _table);
  end
  else
  begin
    PersistHasManyRelation(_table, _idValue, _rttitype, AObject);
    PersistHasOneRelation(_table, _idValue, _rttitype, AObject);
    GetStrategy.Delete(_rttitype, AObject, _table);
  end;
  ClearOID(AObject);
  SetObjectStatus(AObject, osDeleted, false);
  _validateable.OnAfterDelete;
  GetLogger.ExitLevel('Delete');
end;

function TSession.InternalInsert(AObject: TObject;
  AValidaetable: TdormValidateable): TValue;
var
  _Type: TRttiType;
  _table: TMappingTable;
  _idValue: TValue;
  _validateable: TdormValidateable;
begin
  _Type := FCTX.GetType(AObject.ClassInfo);
  _table := FMappingStrategy.GetMapping(_Type);
  _idValue := GetIdValue(_table.Id, AObject);
  if GetObjectStatus(AObject) = osClean then
    Exit(_idValue);
  GetLogger.EnterLevel('INSERT ' + AObject.ClassName);
  _validateable := WrapAsValidateableObject(AObject);
  try
    _validateable.Validate;
    _validateable.InsertValidate;
    _validateable.OnBeforeInsert;
    if IsNullKey(_table, _idValue) then
    begin
      FLogger.Info('Inserting ' + AObject.ClassName);
      FixBelongsToRelation(_table, _idValue, _Type, AObject);
      Result := GetStrategy.Insert(_Type, AObject, _table);
      _idValue := GetIdValue(_table.Id, AObject);
      if CurrentObjectStatus <> osUnknown then
      begin
        PersistHasManyRelation(_table, _idValue, _Type, AObject);
        PersistHasOneRelation(_table, _idValue, _Type, AObject);
      end
      else
      begin
        InsertHasManyRelation(_table, _idValue, _Type, AObject);
        InsertHasOneRelation(_table, _idValue, _Type, AObject);
      end;

    end
    else
      raise EdormException.CreateFmt('Cannot insert [%s] because OI is not null',
        [AObject.ClassName]);
    SetObjectStatus(AObject, osClean, false);
    _validateable.OnAfterInsert;
  finally
    _validateable.Free;
  end;
  GetLogger.ExitLevel('INSERT ' + AObject.ClassName);
end;

procedure TSession.InternalUpdate(AObject: TObject;
  AValidaetable: TdormValidateable; AOnlyChild: boolean);
var
  _Type: TRttiType;
  _class_name: string;
  _table: TMappingTable;
  _idValue: TValue;
begin
  _Type := FCTX.GetType(AObject.ClassInfo);
  _table := FMappingStrategy.GetMapping(_Type);
  _idValue := GetIdValue(_table.Id, AObject);
  GetLogger.EnterLevel('UPDATE ' + AObject.ClassName);
  AValidaetable.UpdateValidate;
  AValidaetable.OnBeforeUpdate;
  _class_name := _Type.ToString;
  if not IsNullKey(_table, _idValue) then
  begin
    FLogger.Info('Updating ' + AObject.ClassName);
    if not AOnlyChild then
      GetStrategy.Update(_Type, AObject, _table);
    if CurrentObjectStatus <> osUnknown then
    begin
      PersistHasManyRelation(_table, _idValue, _Type, AObject);
      PersistHasOneRelation(_table, _idValue, _Type, AObject);
    end;
  end
  else
    raise EdormException.CreateFmt('Cannot update object without an ID [%s]',
      [_class_name]);
  SetObjectStatus(AObject, osClean, false);
  AValidaetable.OnAfterUpdate;
  GetLogger.ExitLevel('UPDATE ' + AObject.ClassName);
end;

function TSession.IsAlreadyLoaded(ATypeInfo: PTypeInfo; AValue: TValue;
  out AOutObject: TObject): boolean;
begin
  Result := LoadedObjects.TryGetValue(GetLoadedObjectHashCode(ATypeInfo,
    AValue), AOutObject);
end;

function TSession.IsClean(AObject: TObject): boolean;
begin
  Result := GetObjectStatus(AObject) = osClean;
end;

function TSession.IsDirty(AObject: TObject): boolean;
begin
  Result := GetObjectStatus(AObject) = osDirty;
end;

function TSession.IsInTransaction: boolean;
begin
  Result := FPersistStrategy.InTransaction;
end;

function TSession.IsNullKey(ATableMap: TMappingTable;
  const AValue: TValue): boolean;
begin
  Result := TdormUtils.EqualValues(AValue, FIdNullValue);
end;

function TSession.IsObjectStatusAvailable(AObject: TObject): boolean;
begin
  Result := GetObjectStatus(AObject) <> osUnknown;
end;

function TSession.IsUnknown(AObject: TObject): boolean;
begin
  Result := GetObjectStatus(AObject) = osUnknown;
end;

procedure TSession.FixBelongsToRelation(AMappingTable: TMappingTable;
  AIdValue: TValue; ARttiType: TRttiType; AObject: TObject);
var
  _belongs_to: TMappingBelongsTo;
  _parent_type: TRttiType;
  v: TValue;
  ParentObject: TObject;
begin
  GetLogger.EnterLevel('Fixing belongs_to ' + ARttiType.ToString);
  for _belongs_to in AMappingTable.BelongsToList do
  begin
    ParentObject := TdormUtils.GetProperty(AObject, _belongs_to.Name).AsObject;
    if assigned(ParentObject) then
    begin
      _parent_type := FCTX.FindType(Qualified(AMappingTable,
        _belongs_to.OwnerClassName));
      if not assigned(_parent_type) then
        raise Exception.Create('Unknown type ' + _belongs_to.OwnerClassName);
      v := GetIdValue(FMappingStrategy.GetMapping(_parent_type).Id,
        ParentObject);
      TdormUtils.SetField(AObject, _belongs_to.RefFieldName, v);
    end;
  end;
  GetLogger.ExitLevel('Fixing belongs_to ' + ARttiType.ToString);
end;

procedure TSession.StartTransaction;
begin
  GetLogger.EnterLevel('TSession.StartTransaction');
  GetStrategy.StartTransaction;
end;

function TSession.Strategy: IdormPersistStrategy;
begin
  Result := GetStrategy;
end;

procedure TSession.Update(AObject: TObject);
var
  _v: TdormValidateable;
begin
  if IsObjectStatusAvailable(AObject) then
    raise EdormException.Create
      ('Update cannot be called if [ObjStatus] is enabled');
  _v := WrapAsValidateableObject(AObject);
  try
    InternalUpdate(AObject, _v);
  finally
    _v.Free;
  end;
end;

procedure TSession.UpdateAndFree(AObject: TObject);
begin
  if IsObjectStatusAvailable(AObject) then
    raise EdormException.Create
      ('UpdateAndFree cannot be called if [ObjStatus] is enabled');
  Update(AObject);
  FreeAndNil(AObject);
end;

procedure TSession.UpdateChildTypeWithRealListInnerType
  (AMappingTable: TMappingTable;
  var AChildType: TRttiType);
var
  attr: ListOf;
  _type_name: string;
begin
  // When you have an HasMany relations declared as "TPhones = class(TObjectLlist<TPhone>)" dorm
  // needs to know the mapping of the internal object and not of the list object itself. So now we're
  // searching for an attribute named "ListOf" that explain to the RTTI engine what is internal object type
  attr := TdormUtils.GetAttribute<ListOf>(AChildType);
  if assigned(attr) then
  begin
    _type_name := Qualified(AMappingTable, attr.Value);
    AChildType := FCTX.FindType(_type_name);
    if not assigned(AChildType) then
      raise Exception.Create('Unknown type ' + _type_name +
        ' (ListOf ' + attr.Value + ')');
  end;
end;

procedure TSession.UpdateCollection(ACollection: TObject);
var
  Obj: TObject;
  Coll: IWrappedList;
begin
  Coll := WrapAsList(ACollection);
  for Obj in Coll do
    Update(Obj);
end;

procedure TSession.DeleteCollection(ACollection: TObject);
var
  Obj: TObject;
  Coll: IWrappedList;
begin
  Coll := WrapAsList(ACollection);
  for Obj in Coll do
  begin
    Delete(Obj);
  end;
end;

function TSession.Load<T>(ACriteria: ICriteria): T;
var
  Coll: {$IF CompilerVersion > 22}TObjectList<T>{$ELSE}TdormObjectList<T>{$IFEND};
begin
  Result := nil;
  Coll := LoadList<T>(ACriteria);
  try
    if Coll.Count > 1 then
      raise EdormException.Create('Not expected multiple result rows ');
    if Coll.Count = 1 then
      Result := Coll.Extract(Coll[0]);
  finally
    Coll.Free;
  end;
end;
{ TdormObject }

{ TdormObjectList<T> }

{$IF CompilerVersion = 22}


function TdormObjectList<T>.GetElement(Index: Integer): T;
begin
  Result := Items[index];
end;

{$IFEND}


end.
