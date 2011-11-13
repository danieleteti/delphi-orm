unit dorm.DBCreator;

interface

uses
  dorm,
  Classes,
  SysUtils,
  superobject,
  dorm.Commons, Generics.Collections;

type
  TdormDBCreator = class
  protected
    FSession: TSession;
    FMapping: ISuperObject;
    FSQL: TStringList;
    function GetDatabaseFieldDeclaration(const dormFieldMapping
      : TdormFieldMapping): string; virtual; abstract;
    function EnvDesc(ACurrentEnv: TdormEnvironment): String;
  public
    constructor Create(Session: dorm.TSession);
    procedure Execute; virtual; abstract;
    function GetSQLScript: TStringList;
    destructor Destroy; override;
    procedure CreateDataBase(CurrentEnv: TdormEnvironment); virtual;
  end;

  TdormDBCreatorClass = class of TdormDBCreator;

  TdormDBCreatorsRegister = class
  private
    class var Creators: TDictionary<string, TdormDBCreatorClass>;
  public
    class procedure Register(dormDBCreatorClass: TdormDBCreatorClass;
      const ID: String);
    class destructor Destroy;
    class function GetDBCreators: TDictionary<string, TdormDBCreatorClass>;
  end;

implementation

uses
  dorm.adapter.DBExpress.Factory;

{ TdormDBCreator }

constructor TdormDBCreator.Create(Session: dorm.TSession);
begin
  inherited Create;
  FSession := Session;
  FMapping := FSession.GetMapping;
  FSQL := TStringList.Create;
end;

procedure TdormDBCreator.CreateDataBase(CurrentEnv: TdormEnvironment);
begin

end;

destructor TdormDBCreator.Destroy;
begin
  FSession.Free;
  inherited;
end;

function TdormDBCreator.EnvDesc(ACurrentEnv: TdormEnvironment): String;
begin
  if ACurrentEnv = TdormEnvironment.deDevelopment then
    Exit('development');
  if ACurrentEnv = TdormEnvironment.deTest then
    Exit('test');
  if ACurrentEnv = TdormEnvironment.deRelease then
    Exit('release');
end;

function TdormDBCreator.GetSQLScript: TStringList;
begin
  Result := FSQL;
end;

{ TdormDBCreatorsRegister }

class destructor TdormDBCreatorsRegister.Destroy;
begin
  FreeAndNil(Creators);
end;

class function TdormDBCreatorsRegister.GetDBCreators
  : TDictionary<string, TdormDBCreatorClass>;
begin
  Result := Creators;
end;

class procedure TdormDBCreatorsRegister.Register(dormDBCreatorClass
  : TdormDBCreatorClass; const ID: String);
begin
  if not Assigned(Creators) then
  begin
    Creators := TDictionary<String, TdormDBCreatorClass>.Create;
  end;
  Creators.Add(ID, dormDBCreatorClass);
end;

end.
