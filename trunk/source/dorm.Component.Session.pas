unit dorm.Component.Session;

interface

uses
  System.SysUtils, System.Classes, dorm, dorm.Commons, dorm.Filters,
  System.Rtti, System.Generics.Collections;

type
  TdormSession = class(TComponent)
  private
    FConfigFileName: TFileName;
    FMappingFileName: TFileName;
    FEnvironment: TdormEnvironment;
    FUseMappingFile: boolean;
    FOnAfterPersistObject: TdormSessionPersistEvent;
    FOnBeforePersistObject: TdormSessionPersistEvent;
    procedure SetConfigFileName(const Value: TFileName);
    procedure SetMappingFileName(const Value: TFileName);
    procedure SetEnvironment(const Value: TdormEnvironment);
    procedure SetUseMappingFile(const Value: boolean);

  protected
    FSession: TSession;
    procedure Loaded; override;

  public
    constructor Create(Aowner: TComponent); override;
    procedure Open;
    procedure Close;
    procedure CommitTransaction;
    procedure CommitAndRestart;
    procedure RollbackTransaction;
    procedure StartTransaction;
    function GetInternalSession: TSession;
    // some methods published from the internal session
    procedure Persist(AObject: TObject);
    procedure PersistCollection(ACollection: TObject);
    function GetObjectStatus(AObject: TObject): TdormObjectStatus;
    function IsObjectStatusAvailable(AObject: TObject): boolean;
    procedure SetObjectStatus(AObject: TObject; AStatus: TdormObjectStatus;
      ARaiseExceptionIfNotExists: boolean = true);
    function IsDirty(AObject: TObject): boolean;
    function IsClean(AObject: TObject): boolean;
    function IsUnknown(AObject: TObject): boolean;
    procedure LoadRelations(AObject: TObject;
      ARelationsSet: TdormRelations = [drBelongsTo, drHasMany, drHasOne]
      ); overload;
    procedure LoadRelationsForEachElement(AList: TObject;
      ARelationsSet: TdormRelations = [drBelongsTo, drHasMany, drHasOne]
      ); overload;
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
    function Count(AClassType: TClass): Int64;
    procedure DeleteAll(AClassType: TClass);

  published
    property UseMappingFile: boolean read FUseMappingFile
      write SetUseMappingFile;
    property Environment: TdormEnvironment read FEnvironment
      write SetEnvironment;
    property ConfigFileName: TFileName read FConfigFileName
      write SetConfigFileName;
    property MappingFileName: TFileName read FMappingFileName
      write SetMappingFileName;
    // events
    property OnBeforePersistObject: TdormSessionPersistEvent
      read FOnBeforePersistObject write FOnBeforePersistObject;
    property OnAfterPersistObject: TdormSessionPersistEvent
      read FOnAfterPersistObject write FOnAfterPersistObject;
  end;

implementation

uses
  dorm.Loggers, dorm.adapters;
{ TdormSession }

procedure TdormSession.StartTransaction;
begin
  GetInternalSession.StartTransaction;
end;

procedure TdormSession.Close;
begin
  FSession.Free;
end;

procedure TdormSession.CommitAndRestart;
begin
  GetInternalSession.Commit(true);
end;

procedure TdormSession.CommitTransaction;
begin
  GetInternalSession.Commit(false);
end;

function TdormSession.Count(AClassType: TClass): Int64;
begin
  Result := GetInternalSession.Count(AClassType);
end;

constructor TdormSession.Create(Aowner: TComponent);
begin
  inherited;
  FUseMappingFile := false;
  FConfigFileName := '';
  FMappingFileName := '';
end;

procedure TdormSession.DeleteAll(AClassType: TClass);
begin
  GetInternalSession.DeleteAll(AClassType);
end;

procedure TdormSession.DisableLazyLoad(AClass: TClass;
  const APropertyName: string);
begin
  GetInternalSession.DisableLazyLoad(AClass, APropertyName);
end;

procedure TdormSession.EnableLazyLoad(AClass: TClass;
  const APropertyName: string);
begin
  GetInternalSession.EnableLazyLoad(AClass, APropertyName);
end;

function TdormSession.GetInternalSession: TSession;
begin
  Result := FSession;
end;

function TdormSession.GetObjectStatus(AObject: TObject): TdormObjectStatus;
begin
  Result := GetInternalSession.GetObjectStatus(AObject);
end;

function TdormSession.IsClean(AObject: TObject): boolean;
begin
  Result := GetInternalSession.IsClean(AObject);
end;

function TdormSession.IsDirty(AObject: TObject): boolean;
begin
  Result := GetInternalSession.IsDirty(AObject);
end;

function TdormSession.IsObjectStatusAvailable(AObject: TObject): boolean;
begin
  Result := GetInternalSession.IsObjectStatusAvailable(AObject);
end;

function TdormSession.IsUnknown(AObject: TObject): boolean;
begin
  Result := GetInternalSession.IsUnknown(AObject);
end;

function TdormSession.Load<T>(ACriteria: ICriteria): T;
begin
  Result := GetInternalSession.Load<T>(ACriteria);
end;

function TdormSession.Load<T>(const Value: TValue; AObject: TObject): boolean;
begin
  Result := GetInternalSession.Load<T>(Value, AObject);
end;

function TdormSession.Load<T>(const Value: TValue): T;
begin
  Result := GetInternalSession.Load<T>(Value);
end;

procedure TdormSession.Loaded;
begin
  inherited;
end;

procedure TdormSession.LoadList<T>(Criteria: ICriteria; AObject: TObject);
begin
  GetInternalSession.LoadList<T>(Criteria, AObject);
end;

function TdormSession.LoadList<T>(Criteria: ICriteria):

{$IF CompilerVersion > 22}TObjectList<T>{$ELSE}TdormObjectList<T>{$IFEND};
begin
  Result := GetInternalSession.LoadList<T>(Criteria);
end;

procedure TdormSession.LoadRelations(AObject: TObject;
  ARelationsSet: TdormRelations);
begin
  GetInternalSession.LoadRelations(AObject, ARelationsSet);
end;

procedure TdormSession.LoadRelationsForEachElement(AList: TObject;
  ARelationsSet: TdormRelations);
begin
  GetInternalSession.LoadRelationsForEachElement(AList, ARelationsSet);
end;

procedure TdormSession.Open;
begin
  if UseMappingFile then
    FSession := TSession.CreateConfigured
      (TStreamReader.Create(self.FConfigFileName),
      TStreamReader.Create(self.FMappingFileName), FEnvironment)
  else
    FSession := TSession.CreateConfigured
      (TStreamReader.Create(self.FConfigFileName), FEnvironment);
  FSession.OnBeforePersistObject := FOnBeforePersistObject;
  FSession.OnAfterPersistObject := FOnAfterPersistObject;
  FSession.StartTransaction;
end;

procedure TdormSession.Persist(AObject: TObject);
begin
  GetInternalSession.Persist(AObject);
end;

procedure TdormSession.PersistCollection(ACollection: TObject);
begin
  GetInternalSession.PersistCollection(ACollection);
end;

procedure TdormSession.RollbackTransaction;
begin
  GetInternalSession.Rollback;
end;

procedure TdormSession.SetConfigFileName(const Value: TFileName);
begin
  FConfigFileName := Value;
end;

procedure TdormSession.SetEnvironment(const Value: TdormEnvironment);
begin
  FEnvironment := Value;
end;

procedure TdormSession.SetMappingFileName(const Value: TFileName);
begin
  FMappingFileName := Value;
end;

procedure TdormSession.SetObjectStatus(AObject: TObject;
  AStatus: TdormObjectStatus; ARaiseExceptionIfNotExists: boolean);
begin
  GetInternalSession.SetObjectStatus(AObject, AStatus,
    ARaiseExceptionIfNotExists);
end;

procedure TdormSession.SetUseMappingFile(const Value: boolean);
begin
  FUseMappingFile := Value;
end;

end.
