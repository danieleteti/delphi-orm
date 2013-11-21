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

unit dorm.adapter.FireDac.Facade;

interface

uses
  uADGUIxIntf, uADGUIxFormsWait,
  uADStanIntf, uADStanOption, uADStanError, uADPhysIntf, uADStanDef,
  uADStanPool, uADStanAsync, uADPhysManager, uADStanParam, uADDatSManager,
  uADDAptIntf, uADDAptManager, uADCompClient, Data.DB, uADCompDataSet,
  uADCompGUIx,
  superobject, Classes;

type
  TFireDacFacade = class
  protected
  var
    FFireDacDatabase: TADConnection;

  protected
    FCurrentTransaction      : TADTransaction;
    FParameters              : TStringList;
    FDriverID                : string;
    function NewStatement: TADCommand;
    function NewQuery: TADQuery;

  public
    constructor Create(AParameters: TStringList);
    destructor Destroy; override;
    function GetConnection: TADConnection;
    function GetCurrentTransaction: TADTransaction;
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    function Execute(ASQL: string): Int64; overload;
    function Execute(ASQLCommand: TADCommand): Int64; overload;
    function Execute(ASQLCommand: TADQuery): Int64; overload;
    function ExecuteQuery(ASQLCommand: TADQuery): TADQuery; overload;
    function Prepare(ASQL: string): TADQuery;
  end;

implementation

uses
  sysutils,
  dorm.Commons;

{ Factory }

procedure TFireDacFacade.StartTransaction;
begin
  GetConnection;
  FCurrentTransaction.StartTransaction;
end;

procedure TFireDacFacade.CommitTransaction;
begin
  FCurrentTransaction.Commit;
end;

procedure TFireDacFacade.RollbackTransaction;
begin
  FCurrentTransaction.RollBack;
end;

function TFireDacFacade.Execute(ASQL: string): Int64;
var
  Cmd: TADCommand;
begin
  Cmd := NewStatement;
  try
    Cmd.CommandText.Text := ASQL;
    Cmd.Execute;
    Result := Cmd.RowsAffected;
  finally
    Cmd.Free;
  end;
end;

function TFireDacFacade.Prepare(ASQL: string): TADQuery;
var
  Cmd: TADQuery;
begin
  Cmd := NewQuery;
  try
    Cmd.SQL.Text := ASQL;
    //Cmd.Prepare;
  except
    FreeAndNil(Cmd);
    raise;
  end;
  Result := Cmd;
end;

constructor TFireDacFacade.Create(AParameters: TStringList);
begin
  inherited Create;
  FParameters:=TStringList.Create;
  FParameters.Text:=AParameters.Text;
end;

destructor TFireDacFacade.Destroy;
begin
  if assigned(FFireDacDatabase) then
  begin
    if assigned(FCurrentTransaction) and (FCurrentTransaction.Connection.InTransaction) then
      FCurrentTransaction.RollBack;

    FFireDacDatabase.Connected := False;
    FCurrentTransaction.Free;
    FFireDacDatabase.Free;
  end;

  FParameters.Free;

  inherited;
end;

function TFireDacFacade.Execute(ASQLCommand: TADCommand): Int64;
begin
  ASQLCommand.Execute;
  Result := ASQLCommand.RowsAffected;
end;

function TFireDacFacade.ExecuteQuery(ASQLCommand: TADQuery): TADQuery;
begin
  Result:=ASQLCommand;
  ASQLCommand.Execute;
end;

function TFireDacFacade.GetConnection: TADConnection;
begin
  if FFireDacDatabase = nil then
  begin
    FFireDacDatabase := TADConnection.Create(nil);
    FFireDacDatabase.Params.Text:=FParameters.Text;
    FFireDacDatabase.Connected := True;

    FCurrentTransaction := TADTransaction.Create(nil);
    FCurrentTransaction.Connection:=GetConnection;
    FFireDacDatabase.Transaction:=FCurrentTransaction;
    FFireDacDatabase.UpdateTransaction:=FCurrentTransaction;
  end;
  Result := FFireDacDatabase;
end;

function TFireDacFacade.GetCurrentTransaction: TADTransaction;
begin
  Result := FCurrentTransaction
end;

function TFireDacFacade.NewStatement: TADCommand;
begin
  Result := TADCommand.Create(nil);
  Result.Connection := GetConnection;
  Result.Transaction := FCurrentTransaction;
end;

function TFireDacFacade.NewQuery: TADQuery;
begin
  Result := TADQuery.Create(nil);
  Result.Connection := GetConnection;
  Result.Transaction := FCurrentTransaction;
end;

function TFireDacFacade.Execute(ASQLCommand: TADQuery): Int64;
begin
  ASQLCommand.Execute;
  Result := ASQLCommand.RowsAffected;
end;

end.
