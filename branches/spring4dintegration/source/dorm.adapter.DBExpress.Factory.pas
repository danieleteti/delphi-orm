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

unit dorm.adapter.DBExpress.Factory;

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
  TDBXFactory = class
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
    hostname: string;
  public
    constructor Create(const DBXLibraryName: String; const DriverName: String;
      ConfigurationInfo: ISuperObject);
    destructor Destroy; override;
    function GetConnection: TDBXConnection;
    function Execute(ASQL: string): Int64; overload;
    function Execute(ASQLCommand: TDBXCommand): Int64; overload;
    function ExecuteQuery(ASQLCommand: TDBXCommand): TDBXReader; overload;
    function Prepare(ASQL: string): TDBXCommand;
    property ConnectionProps: TDBXProperties read FConnectionProps;
  end;

implementation

uses
  sysutils;

{ Factory }

function TDBXFactory.Execute(ASQL: string): Int64;
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

function TDBXFactory.Prepare(ASQL: string): TDBXCommand;
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

procedure TDBXFactory.Configure(ConfigurationInfo: ISuperObject);
begin
  database_connection_string := ConfigurationInfo.S
    ['database_connection_string'];
  username := ConfigurationInfo.S['username'];
  password := ConfigurationInfo.S['password'];
  hostname := ConfigurationInfo.S['hostname'];
end;

constructor TDBXFactory.Create(const DBXLibraryName: String;
  const DriverName: String; ConfigurationInfo: ISuperObject);
begin
  inherited Create;
  Configure(ConfigurationInfo);
  FConnectionProps := TDBXProperties.Create;
  FConnectionFactory := TDBXConnectionFactory.GetConnectionFactory;
  FConnectionProps.Add(TDBXPropertyNames.username, username);
  FConnectionProps.Add(TDBXPropertyNames.password, password);
  FConnectionProps.Add(TDBXPropertyNames.HostName, hostname);
  //FConnectionProps.Add('ServerCharSet', 'utf8');
  FConnectionProps.Add(TDBXPropertyNames.Database,
    database_connection_string);
  FConnectionProps.Add(TDBXPropertyNames.DriverName, DriverName);
  FConnectionProps.Add(TDBXPropertyNames.LibraryName, DBXLibraryName);
end;

destructor TDBXFactory.Destroy;
begin
  FConnectionProps.Free;
  if assigned(FDBXConnection) then
  begin
    if FDBXConnection.IsOpen then
      FDBXConnection.Close;
    FDBXConnection.Free;
  end;
  inherited;
end;

function TDBXFactory.Execute(ASQLCommand: TDBXCommand): Int64;
begin
  ASQLCommand.ExecuteUpdate;
  Result := ASQLCommand.RowsAffected;
end;

function TDBXFactory.ExecuteQuery(ASQLCommand: TDBXCommand): TDBXReader;
begin
  raise EdormException.Create('Not implemented');
end;

function TDBXFactory.GetConnection: TDBXConnection;
begin
  if FDBXConnection = nil then
    FDBXConnection := FConnectionFactory.GetConnection(FConnectionProps);
  Result := FDBXConnection;
end;

end.
