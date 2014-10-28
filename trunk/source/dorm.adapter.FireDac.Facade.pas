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

  Contributor: Marco Mottadelli

  28/10/2014 Marco Mottadelli - Manage empty string like null
  ******************************************************************************** }

unit dorm.adapter.FireDAC.Facade;

interface

uses
  System.Classes,
  Data.DB,
  FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.UI,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  FireDAC.Phys,
  FireDAC.Phys.Intf,
  FireDAC.Phys.ODBCBase,
  superobject;

type
  TFireDACFacade = class
  protected
    FConnectionString: string;
    FConnection: TFDConnection;
    FCurrentTransaction: TFDTransaction;
    FDriverLink: TFDPhysODBCBaseDriverLink;
    function NewCommand: TFDCommand;
    procedure SetConnectionParams;
  public
    constructor Create(const AConnectionString: string; DriverLink: TFDPhysODBCBaseDriverLink);
    destructor Destroy; override;
    procedure StartTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    function GetConnection: TFDConnection;
    function GetCurrentTransaction: TFDTransaction;
    function NewQuery: TFDQuery;
    function Execute(ASQL: string): Int64; overload;
    function Execute(ASQLCommand: TFDCommand): Int64; overload;
    function Execute(ASQLQuery: TFDQuery): Int64; overload;

  end;

implementation

uses
  sysutils,
  dorm.Commons;

procedure TFireDACFacade.SetConnectionParams;
var
  SlParams: TStringList;
  i: Integer;
begin
  SlParams := TStringList.Create;
  SlParams.Delimiter := ';';
  SlParams.DelimitedText := FConnectionString;

  for i := 0 to SlParams.Count-1 do
  begin
    FConnection.Params.Add(SlParams.Strings[i]);
  end;

  FreeAndNil(SlParams);
end;

procedure TFireDACFacade.StartTransaction;
begin
  GetConnection;
  FCurrentTransaction.StartTransaction;
end;

procedure TFireDACFacade.CommitTransaction;
begin
  FCurrentTransaction.Commit;
end;

procedure TFireDACFacade.RollbackTransaction;
begin
  FCurrentTransaction.RollBack;
end;

function TFireDACFacade.Execute(ASQL: string): Int64;
var
  Cmd: TFDCommand;
begin
  Cmd := NewCommand;
  try
    Cmd.CommandText.Add(ASQL);
    Cmd.Execute;
    Result := Cmd.RowsAffected;
  finally
    Cmd.Free;
  end;
end;

constructor TFireDACFacade.Create(const AConnectionString: string; DriverLink: TFDPhysODBCBaseDriverLink);
begin
  inherited Create;
  FConnectionString := AConnectionString;
  FDriverLink := DriverLink;
end;

destructor TFireDACFacade.Destroy;
begin
  if Assigned(FConnection) then
  begin
    if Assigned(FCurrentTransaction) and (FCurrentTransaction.Active) then
      FCurrentTransaction.RollBack;

    FConnection.Connected := False;
    FCurrentTransaction.Free;
    FConnection.Free;
  end;
  inherited;
end;

function TFireDACFacade.Execute(ASQLCommand: TFDCommand): Int64;
begin
  ASQLCommand.Execute();
  Result := ASQLCommand.RowsAffected;
end;

function TFireDACFacade.GetConnection: TFDConnection;
begin
  if FConnection = nil then
  begin
    FConnection := TFDConnection.Create(nil);
    SetConnectionParams;
    FCurrentTransaction := TFDTransaction.Create(nil);
    FCurrentTransaction.Connection := GetConnection;
  end;
  Result := FConnection;
end;

function TFireDACFacade.GetCurrentTransaction: TFDTransaction;
begin
  Result := FCurrentTransaction
end;

function TFireDACFacade.NewCommand: TFDCommand;
begin
  Result := TFDCommand.Create(nil);
  Result.Connection := GetConnection;
  Result.Transaction := FCurrentTransaction;
end;

function TFireDACFacade.NewQuery: TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  Result.Connection := GetConnection;
  Result.Transaction := FCurrentTransaction;
  // M.M. 28/10/2014
  Result.OptionsIntf.FormatOptions.StrsEmpty2Null:=True;
end;

function TFireDACFacade.Execute(ASQLQuery: TFDQuery): Int64;
begin
  ASQLQuery.ExecSQL;
  Result := ASQLQuery.RowsAffected;
end;

end.
