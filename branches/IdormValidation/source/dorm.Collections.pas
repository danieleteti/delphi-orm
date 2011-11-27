{ *******************************************************************************
  Copyright 2010-2011 Daniele Teti

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
  TdormCollection = class;

  TdormCollectionEnumerator = class
  private
    FCurrent: TObject;
    FCollection: TdormCollection;
  protected
    FCurrentIndex: Int64;
    constructor Create(Collection: TdormCollection);
  public
    function GetCurrent: TObject;
    function MoveNext: Boolean;
    property Current: TObject read GetCurrent;
  end;

  TdormCollection = class
  private
    FItems: TObjectList<TObject>;
    FChilds: TObjectList<TObject>;
  public
    procedure Sort(AComparer: IComparer<TObject>);
    procedure ReverseSort(AComparer: IComparer<TObject>);
    function GetEnumerator: TdormCollectionEnumerator;
    function Add(const Value: TObject): TdormCollection;
    function First: TObject;
    function Last: TObject;
    procedure Clear;
    function Append(Collection: TdormCollection;
      FreeAfterAppend: Boolean = false): TdormCollection;
    function GetCount: Integer;
    function Remove(const Value: TObject): Integer;
    function Extract(index: Integer): TObject; overload;
    function Extract(Obj: TObject): TObject; overload;
    procedure Delete(index: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function contains(const Value: TObject): Boolean;
    function IndexOf(const Value: TObject): Integer;
    function GetItem(index: Integer): TObject;
    procedure SetItem(index: Integer; const Value: TObject);
    procedure SetOwnsObjects(Value: Boolean);
    function AsObjectList<T: class>(): TObjectList<T>;
    procedure ForEach<T: class>(Proc: TProc<T>);
    function FindFirst<T: class>(Filter: TFunc<T, Boolean>): T;
    constructor Create; virtual;
    destructor Destroy; override;
    property Items[index: Integer]: TObject read GetItem write SetItem; default;
    property Count: Integer read GetCount;
  end;

  TdormComparer = class(TComparer<TObject>)
  protected
    FAttributeName: String;
  public
    function Compare(const Left, Right: TObject): Integer; override;
    constructor Create(const AttributeName: String);
  end;

function NewList(AOwnObjects: Boolean = true): TdormCollection; overload;
function NewList(Objects: array of TObject; AOwnObjects: Boolean = true)
  : TdormCollection; overload;

implementation

uses
  dorm.Utils, dorm.Commons;

function NewList(AOwnObjects: Boolean): TdormCollection; overload;
begin
  Result := TdormCollection.Create;
  Result.SetOwnsObjects(AOwnObjects);
end;

function NewList(Objects: array of TObject; AOwnObjects: Boolean)
  : TdormCollection;
overload

var
  Obj: TObject;
begin
  Result := TdormCollection.Create;
  Result.SetOwnsObjects(AOwnObjects);
  for Obj in Objects do
    Result.Add(Obj);
end;

{ TdormCollection }

function TdormCollection.Add(const Value: TObject): TdormCollection;
begin
  FItems.Add(Value);
  Result := Self;
end;

function TdormCollection.Append(Collection: TdormCollection;
  FreeAfterAppend: Boolean): TdormCollection;
var
  I: Integer;
begin
  Result := Self;
  if Assigned(Collection) then
  begin
    for I := 0 to Collection.Count - 1 do
      Add(Collection[I]);
    if FreeAfterAppend then
      FreeAndNil(Collection);
  end;
end;

function TdormCollection.AsObjectList<T>: TObjectList<T>;
var
  I: Integer;
begin
  if FChilds = nil then
    FChilds := TObjectList<TObject>.Create(true);
  Result := TObjectList<T>.Create(false);
  FChilds.Add(Result);
  for I := 0 to Count - 1 do
  begin
    Result.Add(Items[I] as T);
  end;
end;

procedure TdormCollection.Clear;
begin
  FItems.Clear;
end;

function TdormCollection.contains(const Value: TObject): Boolean;
begin
  Result := FItems.contains(Value);
end;

function TdormCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

constructor TdormCollection.Create;
begin
  inherited Create;
  FItems := TObjectList<TObject>.Create(true);
end;

procedure TdormCollection.Delete(index: Integer);
begin
  FItems.Delete(index);
end;

procedure TdormCollection.DeleteRange(AIndex, ACount: Integer);
begin
  FItems.DeleteRange(AIndex, ACount);
end;

destructor TdormCollection.Destroy;
begin
  if Assigned(FChilds) then
    FChilds.Free;
  FItems.Free;
  inherited;
end;

function TdormCollection.Extract(index: Integer): TObject;
begin
  Result := FItems.Extract(GetItem(index));
end;

function TdormCollection.Extract(Obj: TObject): TObject;
begin
  Result := FItems.Extract(Obj);
end;

function TdormCollection.FindFirst<T>(Filter: TFunc<T, Boolean>): T;
var
  o: TObject;
begin
  for o in FItems do
    if Filter(T(o)) then
      Exit(T(o));
  Result := nil;
end;

function TdormCollection.First: TObject;
begin
  Result := FItems.First;
end;

procedure TdormCollection.ForEach<T>(Proc: TProc<T>);

var
  o: TObject;
begin
  for o in FItems do
    Proc(o);
end;

function TdormCollection.GetEnumerator: TdormCollectionEnumerator;
begin
  Result := TdormCollectionEnumerator.Create(Self);
end;

function TdormCollection.GetItem(index: Integer): TObject;
begin
  if (index >= FItems.Count) then
    raise Exception.Create('Index out of range in TdormCollection');
  Result := FItems.Items[index];
end;

function TdormCollection.IndexOf(const Value: TObject): Integer;
begin
  Result := FItems.IndexOf(Value);
end;

function TdormCollection.Last: TObject;
begin
  Result := FItems.Last;
end;

function TdormCollection.Remove(const Value: TObject): Integer;
begin
  Result := FItems.Remove(Value);
end;

procedure TdormCollection.ReverseSort(AComparer: IComparer<TObject>);
begin
  Sort(AComparer);
  FItems.Reverse;
end;

procedure TdormCollection.SetItem(index: Integer; const Value: TObject);
begin
  FItems.Items[index] := Value;
end;

procedure TdormCollection.SetOwnsObjects(Value: Boolean);
begin
  FItems.OwnsObjects := Value;
end;

procedure TdormCollection.Sort(AComparer: IComparer<TObject>);
begin
  FItems.Sort(AComparer);
end;

{ TdormCollectionEnumerator }

constructor TdormCollectionEnumerator.Create(Collection: TdormCollection);
begin
  inherited Create;
  FCurrentIndex := -1;
  FCollection := Collection;
end;

function TdormCollectionEnumerator.GetCurrent: TObject;
begin
  if FCurrentIndex > -1 then
    Result := FCollection[FCurrentIndex];
end;

function TdormCollectionEnumerator.MoveNext: Boolean;
begin
  if FCurrentIndex < FCollection.Count - 1 then
  begin
    inc(FCurrentIndex);
    Result := true;
  end
  else
    Result := false;
end;

{ TdormComparer }

function TdormComparer.Compare(const Left, Right: TObject): Integer;
var
  _VRight, _VLeft: TValue;
begin
  Result := 0;
  _VLeft := TdormUtils.GetProperty(Left, FAttributeName);
  _VRight := TdormUtils.GetProperty(Right, FAttributeName);
  if _VLeft.IsType<String> then
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

constructor TdormComparer.Create(const AttributeName: String);
begin
  inherited Create;
  FAttributeName := AttributeName;
end;

end.
