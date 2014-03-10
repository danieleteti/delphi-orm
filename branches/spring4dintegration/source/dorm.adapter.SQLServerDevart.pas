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

unit dorm.adapter.SQLServerDevart;

interface

uses
  dorm.Commons,
  classes,
  SysUtils,
  DB,
  SqlExpr,
  dorm.adapter.DBExpress.Factory,
  DBXClient,
  DBXCommon,
  dbclient,
  Rtti,
  dorm,
  superobject,
  TypInfo,
  FMTBcd,
  dorm.Mappings,
  dorm.adapter.SQLServer,
  dorm.Collections;

type
  TSQLServerDevartPersistStrategy = class(TSQLServerPersistStrategy)
  strict protected
    procedure InitializeParamsForCommand(var Command:TDBXCommand;
      AFieldsMapping: TMappingFieldList); override;
    procedure InitializePKParams(var Command:TDBXCommand;
      AFieldsMapping: TMappingFieldList); override;

  protected
    function CreateDBXFactory(Conf: ISuperObject): TDBXFactory; override;
  public
  end;

  TSQLServerDevartIdentity = class(TSQLServerIdentity)
  end;

implementation

uses
  dorm.Utils;

function TSQLServerDevartPersistStrategy.CreateDBXFactory(Conf: ISuperObject)
  : TDBXFactory;
begin
  Result := TDBXFactory.Create('dbexpsda40.dll', 'DevartSQLServer', Conf);
  Result.ConnectionProps.Add(TDBXPropertyNames.VendorLib, 'sqlncli.dll');
  Result.ConnectionProps.Add('Prepared', 'True');
end;

procedure TSQLServerDevartPersistStrategy.InitializeParamsForCommand(
  var Command: TDBXCommand; AFieldsMapping: TMappingFieldList);
var
  field: TMappingField;
begin
  inherited;
  if Command.Parameters.Count = 0 then begin
    for field in AFieldsMapping do begin
      if not field.IsPK then
        Command.Parameters.AddParameter(Command.CreateParameter);
    end;
  end;

end;

procedure TSQLServerDevartPersistStrategy.InitializePKParams(
  var Command: TDBXCommand; AFieldsMapping: TMappingFieldList);
var
  field: TMappingField;
begin
  inherited;
  for field in AFieldsMapping do begin
    if field.IsPK then
      Command.Parameters.AddParameter(Command.CreateParameter);
  end;
end;

initialization

TSQLServerDevartPersistStrategy.register;
TSQLServerDevartIdentity.RegisterClass;

finalization

end.
