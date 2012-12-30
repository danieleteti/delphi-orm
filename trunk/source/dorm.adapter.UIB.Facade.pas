{ *******************************************************************************
  Copyright 2010-2012 Daniele Teti

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

unit dorm.adapter.UIB.Facade;

interface

uses
  UIB,
  superobject;

type
  TUIBFacade = class
  protected
  var
    FUIBDatabase: TUIBDatabase;
  protected
    FCurrentTransaction: TUIBTransaction;
    FDatabaseConnectionString: string;
    FUsername: string;
    FPassword: string;
    FLibraryName: string;
    function NewStatement: TUIBStatement;
    function NewQuery: TUIBQuery;
  public
    constructor Create(const LibraryName: String;
      AUserName, APassword, AConnectionString: String);
    destructor Destroy; override;
    function GetConnection: TUIBDatabase;
    function GetCurrentTransaction: TUIBTransaction;
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    function Execute(ASQL: string): Int64; overload;
    function Execute(ASQLCommand: TUIBStatement): Int64; overload;
    function ExecuteQuery(ASQLCommand: TUIBQuery): TUIBQuery; overload;
    function Prepare(ASQL: string): TUIBQuery;
  end;

implementation

uses
  sysutils,
  dorm.Commons,
  uiblib;

{ Factory }

procedure TUIBFacade.StartTransaction;
begin
  GetConnection;
  FCurrentTransaction.StartTransaction;
end;

procedure TUIBFacade.CommitTransaction;
begin
  FCurrentTransaction.Commit;
end;

procedure TUIBFacade.RollbackTransaction;
begin
  FCurrentTransaction.RollBack;
end;

function TUIBFacade.Execute(ASQL: string): Int64;
var
  Cmd: TUIBStatement;
begin
  Cmd := NewStatement;
  try
    Cmd.SQL.Text := ASQL;
    Cmd.Execute;
    Result := Cmd.RowsAffected;
  finally
    Cmd.Free;
  end;
end;

function TUIBFacade.Prepare(ASQL: string): TUIBQuery;
var
  Cmd: TUIBQuery;
begin
  Cmd := NewQuery;
  try
    Cmd.FetchBlobs := True;
    Cmd.SQL.Text := ASQL;
    Cmd.Prepare;
  except
    FreeAndNil(Cmd);
    raise;
  end;
  Result := Cmd;
end;

constructor TUIBFacade.Create(const LibraryName: String;
  AUserName, APassword, AConnectionString: String);
begin
  inherited Create;
  FLibraryName := LibraryName;
  FDatabaseConnectionString := AConnectionString;
  FUsername := AUserName;
  FPassword := APassword;
end;

destructor TUIBFacade.Destroy;
begin
  if assigned(FUIBDatabase) then
  begin
    if Assigned(FCurrentTransaction) and (FCurrentTransaction.InTransaction) then
      FCurrentTransaction.RollBack;

    FUIBDatabase.Connected := False;
    FCurrentTransaction.Free;
    FUIBDatabase.Free;
  end;
  inherited;
end;

function TUIBFacade.Execute(ASQLCommand: TUIBStatement): Int64;
begin
  ASQLCommand.Execute;
  Result := ASQLCommand.RowsAffected;
end;

function TUIBFacade.ExecuteQuery(ASQLCommand: TUIBQuery): TUIBQuery;
begin
  raise EdormException.Create('Not implemented');
end;

function TUIBFacade.GetConnection: TUIBDatabase;
begin
  if FUIBDatabase = nil then
  begin
    FUIBDatabase := TUIBDatabase.Create(nil);
    FUIBDatabase.LibraryName := FLibraryName;
    FUIBDatabase.DatabaseName := FDatabaseConnectionString;
    FUIBDatabase.username := FUsername;
    FUIBDatabase.password := FPassword;
    FUIBDatabase.CharacterSet := csUTF8; // always unicode
    FUIBDatabase.Connected := True;
    FCurrentTransaction := TUIBTransaction.Create(nil);
    FCurrentTransaction.DataBase := GetConnection;
  end;
  Result := FUIBDatabase;
end;

function TUIBFacade.GetCurrentTransaction: TUIBTransaction;
begin
  Result := FCurrentTransaction
end;

function TUIBFacade.NewStatement: TUIBStatement;
begin
  Result := TUIBStatement.Create(nil);
  Result.Transaction := FCurrentTransaction;
end;

function TUIBFacade.NewQuery: TUIBQuery;
begin
  Result := TUIBQuery.Create(nil);
  Result.Transaction := FCurrentTransaction;
end;

end.
