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

unit dorm.Collections;

interface

uses
  Generics.Defaults,
  Generics.Collections,
  RTTI,
  SysUtils,
  Classes;

type
  TdormComparer<T: class> = class(TComparer<T>)
  protected
    FAttributeName: string;
  public
    function Compare(const Left, Right: T): Integer; override;
    constructor Create(const AttributeName: string);
  end;

  TdormReverseComparer<T: class> = class(TdormComparer<T>)
  public
    function Compare(const Left, Right: T): Integer; override;
  end;

function NewList(AOwnObjects: Boolean = true):
{$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND}; overload;
function NewList(Objects: array of TObject; AOwnObjects: Boolean = true)
  : {$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND}; overload;

implementation

uses
  dorm.Utils,
  dorm.Commons;

function NewList(AOwnObjects: Boolean): {$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND}; overload;
begin
  Result := {$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND}.Create;
  Result.OwnsObjects := AOwnObjects;
end;

function NewList(Objects: array of TObject; AOwnObjects: Boolean)
  : {$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND}; overload;
var
  Obj: TObject;
begin
  Result := {$IF CompilerVersion > 22}TObjectList<TObject>{$ELSE}TdormObjectList<TObject>{$IFEND}.Create;
  Result.OwnsObjects := AOwnObjects;
  for Obj in Objects do
    Result.Add(Obj);
end;

{ TdormComparer }

function TdormComparer<T>.Compare(const Left, Right: T): Integer;
var
  _VRight, _VLeft: TValue;
begin
  Result := 0;
  _VLeft := TdormUtils.GetProperty(Left, FAttributeName);
  _VRight := TdormUtils.GetProperty(Right, FAttributeName);
  if _VLeft.IsType<string> then
  begin
    if _VLeft.AsString < _VRight.AsString then
      Exit(-1);
    if _VLeft.AsString > _VRight.AsString then
      Exit(1);
  end
  else if _VLeft.IsType<Integer> then
  begin
    if _VLeft.AsInteger < _VRight.AsInteger then
      Exit(-1);
    if _VLeft.AsInteger > _VRight.AsInteger then
      Exit(1);
  end
  else if (_VLeft.IsType<TDate>) or (_VLeft.IsType<TDateTime>) then
  begin
    if _VLeft.AsExtended < _VRight.AsExtended then
      Exit(-1);
    if _VLeft.AsExtended > _VRight.AsExtended then
      Exit(1);
  end
  else
    raise EdormException.Create('Unsupported sort type for attribute ' +
      FAttributeName);
end;

constructor TdormComparer<T>.Create(const AttributeName: string);
begin
  inherited Create;
  FAttributeName := AttributeName;
end;

{ TdormReverseComparer<T> }

function TdormReverseComparer<T>.Compare(const Left, Right: T): Integer;
begin
  Result := inherited * -1;
end;

end.
