unit dorm.adapter.ZeosDBO.Facade;

interface

uses
  ZDbcIntfs,
  ZAbstractConnection,
  ZConnection,
  ZDataset,
  superobject
  ;

type
  TZeosDBOFacade = class
  private
    FDatabaseExtraProperties: string;
  protected
  var
    FConnection: TZConnection;

  protected
    FDatabaseConnectionString: string;
    FUsername: string;
    FPassword: string;
    FProtocolName: string;
    function NewStatement: TZQuery;
    function NewQuery: TZQuery;
//    function GetCharsetFromString(const Charset: String): TCharacterSet;
  public
    constructor Create(const ProtocolName: string;
      AUserName, APassword, AConnectionString, ADatabaseExtraProperties: string);
    destructor Destroy; override;

    function GetConnection: TZConnection;
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    function Execute(ASQL: string): Int64; overload;
    function Execute(ASQLCommand: TZQuery): Int64; overload;
    function ExecuteQuery(ASQLCommand: TZQuery): TZQuery; overload;
    function Prepare(ASQL: string): TZQuery;
  end;

implementation

uses
  sysutils,
  dorm.Commons;



{ TUIBFacade }

procedure TZeosDBOFacade.CommitTransaction;
begin
  FConnection.Commit;
end;

constructor TZeosDBOFacade.Create(const ProtocolName: string; AUserName, APassword,
  AConnectionString, ADatabaseExtraProperties: string);
begin
  inherited Create;
  FProtocolName := ProtocolName;
  FDatabaseConnectionString := AConnectionString;
  FUsername := AUserName;
  FPassword := APassword;
  FDatabaseExtraProperties := ADatabaseExtraProperties;
end;

destructor TZeosDBOFacade.Destroy;
begin
  if assigned(FConnection) then
  begin
    if FConnection.InTransaction then
      FConnection.RollBack;

    FConnection.Connected := False;
    Fconnection.Free;
  end;

  inherited;
end;

function TZeosDBOFacade.Execute(ASQLCommand: TZQuery): Int64;
begin
  ASQLCommand.ExecSQL;
  Result := ASQLCommand.RowsAffected;
end;

function TZeosDBOFacade.Execute(ASQL: string): Int64;
var
  Cmd: TZQuery;
begin
  Cmd := NewStatement;

  try
    Cmd.SQL.Text := ASQL;
    Cmd.ExecSQL;
    Result := Cmd.RowsAffected;
  finally
    Cmd.Free;
  end;
end;

function TZeosDBOFacade.ExecuteQuery(ASQLCommand: TZQuery): TZQuery;
begin

end;

function TZeosDBOFacade.GetConnection: TZConnection;
begin
  if FConnection = nil then
  begin
    FConnection := TZConnection.Create(nil);
    FConnection.Protocol := FProtocolName;
    FConnection.Database := FDatabaseConnectionString;
    FConnection.User := FUsername;
    FConnection.Password := FPassword;
    FConnection.Properties.Text := FDatabaseExtraProperties;
//    FConnection.CharacterSet := GetCharsetFromString(FCharSet); // always unicode
    FConnection.Connected := True;
    FConnection.TransactIsolationLevel := tiReadCommitted;
  end;

  Result := FConnection;
end;

function TZeosDBOFacade.NewQuery: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := GetConnection;
end;

function TZeosDBOFacade.NewStatement: TZQuery;
begin
  Result := TZQuery.Create(nil);
  Result.Connection := GetConnection;
end;

function TZeosDBOFacade.Prepare(ASQL: string): TZQuery;
var
  Cmd: TZQuery;
begin
  Cmd := NewQuery;

  try
//    Cmd.FetchBlobs := True;
    Cmd.SQL.Text := ASQL;
    Cmd.Prepare;
  except
    FreeAndNil(Cmd);
    raise;
  end;

  Result := Cmd;
end;

procedure TZeosDBOFacade.RollbackTransaction;
begin
  FConnection.Rollback;
end;

procedure TZeosDBOFacade.StartTransaction;
begin
  GetConnection;
  FConnection.StartTransaction;
end;

end.
