unit dorm.adapter.Firebird.Factory;

interface

uses
  DBXClient,
  DBXCommon,
  dbclient,
  SqlExpr,
  DbxFirebird,
  superobject,
  dorm.Commons;

type
  TFBFactory = class
  protected
  var
    FDBXConnection: TDBXConnection;
    FConnectionProps: TDBXProperties;
    FConnectionFactory: TDBXConnectionFactory;
  private
    procedure Configure(ConfigurationInfo: ISuperObject);
  protected
    database_connection_string: string;
    username: string;
    password: string;
  public
    constructor Create(ConfigurationInfo: ISuperObject);
    destructor Destroy; override;
    function GetConnection: TDBXConnection;
    function Execute(ASQL: string): Int64; overload;
    function Execute(ASQLCommand: TDBXCommand): Int64; overload;
    function ExecuteQuery(ASQLCommand: TDBXCommand): TDBXReader; overload;
    function Prepare(ASQL: string): TDBXCommand;
  end;

implementation

uses
  sysutils;

{ Factory }

function TFBFactory.Execute(ASQL: string): Int64;
var
  Cmd: TDBXCommand;
begin
  Cmd := GetConnection.CreateCommand;
  try
    Cmd.Text := ASQL;
    Cmd.ExecuteUpdate;
    Result := Cmd.RowsAffected;
  finally
    Cmd.Free;
  end;
end;

function TFBFactory.Prepare(ASQL: string): TDBXCommand;
var
  Cmd: TDBXCommand;
begin
  Cmd := GetConnection.CreateCommand;
  try
    Cmd.Text := ASQL;
    Cmd.Prepare;
  except
    FreeAndNil(Cmd);
    raise;
  end;
  Result := Cmd;
end;

procedure TFBFactory.Configure(ConfigurationInfo: ISuperObject);
begin
  database_connection_string := ConfigurationInfo.S['database_connection_string'];
  username := ConfigurationInfo.S['username'];
  password := ConfigurationInfo.S['password'];
end;

constructor TFBFactory.Create(ConfigurationInfo: ISuperObject);
begin
  inherited Create;
  Configure(ConfigurationInfo);
  FConnectionProps := TDBXProperties.Create;
  try
    FConnectionFactory := TDBXConnectionFactory.GetConnectionFactory;
    FConnectionProps.Add(TDBXPropertyNames.username, username);
    FConnectionProps.Add(TDBXPropertyNames.password, password);
    FConnectionProps.Add('ServerCharSet', 'utf8');
    FConnectionProps.Add(TDBXPropertyNames.Database, database_connection_string);
    FConnectionProps.Add(TDBXPropertyNames.DriverName, 'firebird');
    FDBXConnection := FConnectionFactory.GetConnection(FConnectionProps);
  finally
    FConnectionProps.Free;
  end;
end;

destructor TFBFactory.Destroy;
begin
  if assigned(FDBXConnection) then
  begin
    if FDBXConnection.IsOpen then
      FDBXConnection.Close;
    FDBXConnection.Free;
  end;
  inherited;
end;

function TFBFactory.Execute(ASQLCommand: TDBXCommand): Int64;
begin
  ASQLCommand.ExecuteUpdate;
  Result := ASQLCommand.RowsAffected;
end;

function TFBFactory.ExecuteQuery(ASQLCommand: TDBXCommand): TDBXReader;
begin
  raise EdormException.Create('Not implemented');
end;

function TFBFactory.GetConnection: TDBXConnection;
begin
  Result := FDBXConnection;
end;

end.
