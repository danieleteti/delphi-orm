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

unit dorm.UOW;

interface

uses
  rtti,
  dorm.Commons,
  dorm.Collections,
  Generics.Collections,
  dorm.InterposedObject;

type
  TdormUOW = class
  protected
    FUOWInsert, FUOWUpdate, FUOWDelete:
{$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND};
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddInsertOrUpdate(Obj: TObject;
      const OIDPropertyName: string = 'ID'): TdormUOW;
    function AddDelete(Obj: TObject): TdormUOW;
    function AddInsert(Obj: TObject): TdormUOW;
    function AddUpdate(Obj: TObject): TdormUOW;
    function GetUOWInsert:
{$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND};
    function GetUOWUpdate:
{$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND};
    function GetUOWDelete:
{$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND};
    procedure Clear;
    procedure FreeDeleted;
  end;

implementation

uses
  dorm.Utils,
  SysUtils;

{ TdormUOW }

function TdormUOW.AddDelete(Obj: TObject): TdormUOW;
var
  o: TObject;
begin
  if FUOWInsert.IndexOf(Obj) > -1 then
  begin
    o := FUOWInsert.Extract(Obj);
    FreeAndNil(o);
  end
  else if FUOWUpdate.IndexOf(Obj) > -1 then
  begin
    o := FUOWUpdate.Extract(Obj);
    FUOWDelete.Add(o);
  end
  else
  begin
    FUOWDelete.Add(Obj);
  end;
  Result := Self;
end;

function TdormUOW.AddInsert(Obj: TObject): TdormUOW;
// var
// v: TValue;
begin
  Result := Self;
  if (FUOWInsert.IndexOf(Obj) > -1) then
    Exit;
  FUOWInsert.Add(Obj)
end;

function TdormUOW.AddInsertOrUpdate(Obj: TObject; const OIDPropertyName: string)
  : TdormUOW;
var
  v: TValue;
begin
  if (FUOWInsert.IndexOf(Obj) > -1) or (FUOWUpdate.IndexOf(Obj) > -1) then
  begin
    Result := Self;
    Exit;
  end;

  v := TdormUtils.GetProperty(Obj, OIDPropertyName);
  if v.IsType<Int64> then
  begin
    if v.AsInt64 = 0 then
      FUOWInsert.Add(Obj)
    else
      FUOWUpdate.Add(Obj);
  end
  else if v.IsType<string> then
  begin
    if v.AsString = EmptyStr then
      FUOWInsert.Add(Obj)
    else
      FUOWUpdate.Add(Obj);
  end
  else
    raise EdormException.Create
      ('Invalid key type in TdormUOW.AddInsertOrUpdate');
  Result := Self;
end;

function TdormUOW.AddUpdate(Obj: TObject): TdormUOW;
begin
  Result := Self;
  if (FUOWUpdate.IndexOf(Obj) > -1) then
    Exit;
  FUOWUpdate.Add(Obj);
end;

procedure TdormUOW.Clear;
begin
  FUOWInsert.Clear;
  FUOWUpdate.Clear;
  FUOWDelete.Clear; // this DONT delete also the objects
end;

constructor TdormUOW.Create;
begin
  inherited;
  FUOWInsert := NewList(false);
  FUOWUpdate := NewList(false);
  FUOWDelete := NewList(false);
  // check issue 24 (http://code.google.com/p/delphi-orm/issues/detail?id=24)
end;

destructor TdormUOW.Destroy;
begin
  FUOWInsert.Free;
  FUOWUpdate.Free;
  FUOWDelete.Free;
  inherited;
end;

procedure TdormUOW.FreeDeleted;
var
  o: TObject;
begin
  while FUOWDelete.Count > 0 do
  begin
    o := FUOWDelete[0];
    FUOWDelete.Delete(0);
    FreeAndNil(o);
  end;
end;

function TdormUOW.GetUOWDelete:
{$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND};
begin
  Result := FUOWDelete;
end;

function TdormUOW.GetUOWInsert:
{$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND};
begin
  Result := FUOWInsert;
end;

function TdormUOW.GetUOWUpdate:
{$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND};
begin
  Result := FUOWUpdate;
end;

end.
