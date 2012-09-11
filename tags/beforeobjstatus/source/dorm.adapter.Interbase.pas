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

unit dorm.adapter.Interbase;

interface

uses
  dbxinterbase,
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
  dorm.adapter.Firebird,
  dorm.Collections;

type
  TInterbasePersistStrategy = class(TFirebirdPersistStrategy)
  protected
    function CreateDBXFactory(Conf: ISuperObject): TDBXFactory; override;
  end;

  TInterbaseTableSequence = class(TFirebirdTableSequence)
  end;

implementation

uses
  dorm.Utils;

function TInterbasePersistStrategy.CreateDBXFactory(Conf: ISuperObject)
  : TDBXFactory;
begin
  Result := TDBXFactory.Create('dbxint.dll', 'interbase', Conf);
end;

initialization

TInterbasePersistStrategy.register;
TInterbaseTableSequence.RegisterClass;

finalization

end.
