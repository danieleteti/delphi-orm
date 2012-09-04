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

unit dorm.Commons;

interface

uses
  RTTI,
  Classes,
  superobject,
  Generics.Collections,
  SysUtils,
  TypInfo,
  dorm.Mappings,
  dorm.Filters,
  dorm.Collections,
  dorm.Utils,
  dorm.Mappings.Strategies;

type
  TDuckTypedList = class;

  EdormException = class(Exception)

  end;

  EdormValidationException = class(EdormException)

  end;

  TdormEnvironment = (deDevelopment, deTest, deRelease);
  TdormObjectOwner = (ooItself, ooParent);
  TdormSaveType = (stAllGraph, stSingleObject);
  TdormRelations = set of (drBelongsTo, drHasMany, drHasOne);
  TdormFillOptions = set of (CallAfterLoadEvent);

  TdormInterfacedObject = class(TInterfacedObject)
    constructor Create; virtual;
  end;

  IdormLogger = interface
    ['{3501FE53-3781-4F6F-BE6C-80A2309D8D94}']
    procedure EnterLevel(const Value: string);
    procedure ExitLevel(const Value: string);
    procedure Error(const Value: string);
    procedure Warning(const Value: string);
    procedure Info(const Value: string);
    procedure Debug(const Value: string);
  end;

  IDataBaseBuilder = interface
    ['{71C310B8-FB56-40E6-AED4-D3104CDC9069}']
    procedure Execute;
  end;

  IList = interface
    ['{2A1BCB3C-17A2-4F8D-B6FB-32B2A1BFE840}']
    function Add(const Value: TObject): Integer;
    procedure Clear;
    function Count: Integer;
    function GetItem(index: Integer): TObject;
  end;

  IdormPersistStrategy = interface
    ['{04A7F5C2-9B9B-4259-90C2-F23894189717}']
    function Insert(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable): TValue;
    function Update(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable): TValue;
    function Delete(ARttiType: TRttiType; AObject: TObject; AMappingTable: TMappingTable): TObject;
    function Load(ARttiType: TRttiType; AMappingTable: TMappingTable; const Value: TValue;
      AObject: TObject): boolean;
      overload;
    function Load(ARttiType: TRttiType; AMappingTable: TMappingTable;
      AMappingRelationField: TMappingField; const Value: TValue; AObject: TObject)
      : boolean; overload;
    procedure DeleteAll(AMappingTable: TMappingTable);
    function Count(AMappingTable: TMappingTable): Int64;

    // function List(ARttiType: TRttiType; ATableName: string;
    // AMappingFields: TMappingFieldList;
    // AdormSearchCriteria: ICustomCriteria): TObjectList<TObject>; overload;
    // procedure FillList(AList: TObject; ARttiType: TRttiType;
    // ATableName: string; AMappingFields: TMappingFieldList;
    // AdormSearchCriteria: ICustomCriteria); overload;
    procedure LoadList(AList: TObject; ARttiType: TRttiType;
      AMappingTable: TMappingTable; ACriteria: ICriteria); overload;
    function GetLastInsertOID: TValue;
    procedure ConfigureStrategy(ConfigurationInfo: ISuperObject);
    procedure InitStrategy;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function InTransaction: boolean;
    procedure SetLogger(ALogger: IdormLogger);
    function RawExecute(SQL: string): Int64;
    function ExecuteAndGetFirst(SQL: string): Int64;
    function EscapeString(const Value: string): string;
    function EscapeDate(const Value: TDate): string;
    function EscapeDateTime(const Value: TDate): string;
    function GetSelectSQL(ACriteria: ICriteria; AMappingTable: TMappingTable): string;
    function GetDatabaseBuilder(AEntities: TList<String>; AMappings: ICacheMappingStrategy): IDataBaseBuilder;

  end;

  TdormListEnumerator = class(TEnumerator<TObject>)
  protected
    FPosition: Int64;
    FDuckTypedList: TDuckTypedList;
  protected
    function DoGetCurrent: TObject; override;
    function DoMoveNext: boolean; override;
  public
    constructor Create(ADuckTypedList: TDuckTypedList);
  end;

  IdormKeysGenerator = interface
    ['{476D88A6-E7A1-48A2-8673-C2B646A2E7F4}']
    function NewStringKey(const Entity: string): string;
    function NewIntegerKey(const Entity: string): UInt64;
    procedure SetPersistStrategy(const PersistentStrategy
      : IdormPersistStrategy);
  end;

  IWrappedList = interface
    ['{B60AF5A6-7C31-4EAA-8DFB-D8BD3E112EE7}']
    function Count: Integer;
    function GetItem(const index: Integer): TObject;
    procedure Add(const AObject: TObject);
    procedure Clear;
    function GetEnumerator: TdormListEnumerator;
  end;

  TDuckTypedList = class(TInterfacedObject, IWrappedList)
  protected
    FObjectAsDuck: TObject;
    FAddMethod: TRttiMethod;
    FClearMethod: TRttiMethod;
    FCountProperty: TRttiProperty;
    FGetItemMethod: TRttiMethod;
    function Count: Integer;
    function GetItem(const index: Integer): TObject;
    procedure Add(const AObject: TObject);
    procedure Clear;
  public
    constructor Create(AObjectAsDuck: TObject);
    destructor Destroy; override;
    function GetEnumerator: TdormListEnumerator;
  end;

function GetPKMappingIndex(const AMappingFields: TMappingFieldList): Integer;
function GetMappingRelationByPropertyName(ARelationList: TMappingRelationList;
  const APropertyName: string): TMappingRelation;
function GetMappingBelongsToByPropertyName(ARelationList: TMappingBelongsToList;
  const APropertyName: string): TMappingBelongsTo;
function GetSelectFieldsList(AMapping: TMappingFieldList;
  AWithPrimaryKey: boolean): string;

function WrapAsList(const AObject: TObject): IWrappedList;

implementation

function GetSelectFieldsList(AMapping: TMappingFieldList;
  AWithPrimaryKey: boolean): string;
var
  field: TMappingField;
begin
  Result := '';
  if AWithPrimaryKey then
    for field in AMapping do
    begin
      Result := Result + ',"' + field.FieldName + '"';
    end
  else
    for field in AMapping do
    begin
      if not field.IsPK then
        Result := Result + ',"' + field.FieldName + '"';
    end;
  System.Delete(Result, 1, 1);
end;

function GetMappingRelationByPropertyName(ARelationList: TMappingRelationList;
  const APropertyName: string): TMappingRelation;
var
  _relation: TMappingRelation;
begin
  Result := nil;
  for _relation in ARelationList do
    if CompareText(_relation.Name, APropertyName) = 0 then
    begin
      Result := _relation;
      Break;
    end;
end;

function GetMappingBelongsToByPropertyName(ARelationList: TMappingBelongsToList;
  const APropertyName: string): TMappingBelongsTo;
var
  _relation: TMappingBelongsTo;
begin
  Result := nil;
  for _relation in ARelationList do
    if CompareText(_relation.Name, APropertyName) = 0 then
    begin
      Result := _relation;
      Break;
    end;
end;

function GetPKMappingIndex(const AMappingFields: TMappingFieldList): Integer;
var
  I: Integer;
begin
  for I := 0 to AMappingFields.Count - 1 do
    if AMappingFields[I].IsPK then
      Exit(I);
  Exit(-1);
end;

{ TdormInterfacedObject }

constructor TdormInterfacedObject.Create;
begin
  inherited Create;
end;

constructor TdormListEnumerator.Create(ADuckTypedList: TDuckTypedList);
begin
  inherited Create;
  FDuckTypedList := ADuckTypedList;
  FPosition := -1;
end;

function TdormListEnumerator.DoGetCurrent: TObject;
begin
  if FPosition > -1 then
    Result := FDuckTypedList.GetItem(FPosition)
  else
    raise Exception.Create('Enumerator error: Call MoveNext first');
end;

function TdormListEnumerator.DoMoveNext: boolean;
begin
  if FPosition < FDuckTypedList.Count - 1 then
  begin
    Inc(FPosition);
    Result := True;
  end
  else
    Result := false;
end;

function TDuckTypedList.GetEnumerator: TdormListEnumerator;
begin
  Result := TdormListEnumerator.Create(self);
end;

procedure TDuckTypedList.Add(const AObject: TObject);
begin
  FAddMethod.Invoke(FObjectAsDuck, [AObject]);
end;

procedure TDuckTypedList.Clear;
begin
  FClearMethod.Invoke(FObjectAsDuck, []);
end;

function TDuckTypedList.Count: Integer;
begin
  Result := FCountProperty.GetValue(FObjectAsDuck).AsInteger;
end;

constructor TDuckTypedList.Create(AObjectAsDuck: TObject);
begin
  inherited Create;
  FObjectAsDuck := AObjectAsDuck;
  FAddMethod := TdormUtils.ctx.GetType(AObjectAsDuck.ClassInfo)
    .GetMethod('Add');
  if not Assigned(FAddMethod) then
    raise EdormException.Create('Cannot find method "Add" in the duck object');
  FClearMethod := TdormUtils.ctx.GetType(AObjectAsDuck.ClassInfo)
    .GetMethod('Clear');
  if not Assigned(FClearMethod) then
    raise EdormException.Create
      ('Cannot find method "Clear" in the duck object');
  FGetItemMethod := nil;
{$IF CompilerVersion >= 23}
  FGetItemMethod := TdormUtils.ctx.GetType(AObjectAsDuck.ClassInfo)
    .GetIndexedProperty('Items').ReadMethod;
{$IFEND}
  if not Assigned(FGetItemMethod) then
    FGetItemMethod := TdormUtils.ctx.GetType(AObjectAsDuck.ClassInfo)
      .GetMethod('GetItem');
  if not Assigned(FGetItemMethod) then
    FGetItemMethod := TdormUtils.ctx.GetType(AObjectAsDuck.ClassInfo)
      .GetMethod('GetElement');
  if not Assigned(FGetItemMethod) then
    raise EdormException.Create
      ('Cannot find method Indexed property "Items" or method "GetItem" or method "GetElement" in the duck object');
  FCountProperty := TdormUtils.ctx.GetType(AObjectAsDuck.ClassInfo)
    .GetProperty('Count');
  if not Assigned(FCountProperty) then
    raise EdormException.Create
      ('Cannot find property "Count" in the duck object');
end;

destructor TDuckTypedList.Destroy;
begin

  inherited;
end;

function TDuckTypedList.GetItem(const index: Integer): TObject;
begin
  Result := FGetItemMethod.Invoke(FObjectAsDuck, [index]).AsObject;
end;

function WrapAsList(const AObject: TObject): IWrappedList;
begin
  Result := TDuckTypedList.Create(AObject);
end;

end.
