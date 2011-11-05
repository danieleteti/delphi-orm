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

unit dorm.Utils;

interface

uses
  RTTI,
  DB, dorm.Commons;

type
  TdormUtils = class sealed
  private
    class var ctx: TRttiContext;
  public
    class function MethodCall(AObject: TObject; AMethodName: String;
      AParameters: array of TValue): TValue; static;
    class procedure SetProperty(Obj: TObject; const PropertyName: string;
      const Value: TValue); static;
    class procedure ObjectToDataSet(Obj: TObject; Field: TField;
      var Value: Variant);
    class procedure DatasetToObject(Dataset: TDataset; Obj: TObject);
    class function GetProperty(Obj: TObject;
      const PropertyName: string): TValue;
    class function GetField(Obj: TObject; const PropertyName: string): TValue;
    class procedure SetField(Obj: TObject; const PropertyName: string;
      const Value: TValue);
    class function Clone(Obj: TObject): TObject; static;
    class procedure CopyObject(SourceObj, TargetObj: TObject); static;
    class function CreateObject(ARttiType: TRttiType): TObject; static;

  end;

function FieldFor(const PropertyName: string): string; inline;

implementation

uses
  SysUtils,
  Classes, dorm.Collections;

class function TdormUtils.MethodCall(AObject: TObject; AMethodName: String;
  AParameters: array of TValue): TValue;
var
  m: TRttiMethod;
begin
  m := ctx.GetType(AObject.ClassInfo).GetMethod('Add');
  if Assigned(m) then
    Result := m.Invoke(AObject, AParameters)
  else
    raise EdormException.Create('Cannot find method "ADD" in the object');
end;

function FieldFor(const PropertyName: string): string; inline;
begin
  Result := 'F' + PropertyName;
end;

class function TdormUtils.GetField(Obj: TObject;
  const PropertyName: string): TValue;
var
  Field: TRttiField;
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]',
      [ARttiType.ToString]);
  Field := ARttiType.GetField(FieldFor(PropertyName));
  if Assigned(Field) then
    Result := Field.GetValue(Obj)
  else
  begin
    Prop := ARttiType.GetProperty(PropertyName);
    if not Assigned(Prop) then
      raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]',
        [ARttiType.ToString, PropertyName]);
    Result := Prop.GetValue(Obj);
  end;
end;

class function TdormUtils.GetProperty(Obj: TObject;
  const PropertyName: string): TValue;
var
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]',
      [ARttiType.ToString]);
  Prop := ARttiType.GetProperty(PropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]',
      [ARttiType.ToString, PropertyName]);
  if Prop.IsReadable then
    Result := Prop.GetValue(Obj)
  else
    raise Exception.CreateFmt('Property is not readable [%s.%s]',
      [ARttiType.ToString, PropertyName]);
end;

class procedure TdormUtils.SetField(Obj: TObject; const PropertyName: string;
  const Value: TValue);
var
  Field: TRttiField;
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]',
      [ARttiType.ToString]);
  Field := ARttiType.GetField(FieldFor(PropertyName));
  if Assigned(Field) then
    Field.SetValue(Obj, Value)
  else
  begin
    Prop := ARttiType.GetProperty(PropertyName);
    if Assigned(Prop) then
      Prop.SetValue(Obj, Value)
    else
      raise Exception.CreateFmt('Cannot get RTTI for field or property [%s.%s]',
        [ARttiType.ToString, PropertyName]);
  end;
end;

class procedure TdormUtils.SetProperty(Obj: TObject; const PropertyName: string;
  const Value: TValue);
var
  Prop: TRttiProperty;
  ARttiType: TRttiType;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  if not Assigned(ARttiType) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]',
      [ARttiType.ToString]);
  Prop := ARttiType.GetProperty(PropertyName);
  if not Assigned(Prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]',
      [ARttiType.ToString, PropertyName]);
  if Prop.IsWritable then
    Prop.SetValue(Obj, Value)
  else
    raise Exception.CreateFmt('Property is not writeable [%s.%s]',
      [ARttiType.ToString, PropertyName]);
end;

class procedure TdormUtils.ObjectToDataSet(Obj: TObject; Field: TField;
  var Value: Variant);
begin
  Value := GetProperty(Obj, Field.FieldName).AsVariant;
end;

class procedure TdormUtils.DatasetToObject(Dataset: TDataset; Obj: TObject);
var
  ARttiType: TRttiType;
  props: TArray<TRttiProperty>;
  Prop: TRttiProperty;
  f: TField;
begin
  ARttiType := ctx.GetType(Obj.ClassType);
  props := ARttiType.GetProperties;
  for Prop in props do
    if not SameText(Prop.Name, 'ID') then
    begin
      f := Dataset.FindField(Prop.Name);
      if Assigned(f) and not f.ReadOnly then
      begin
        if f is TIntegerField then
          SetProperty(Obj, Prop.Name, TIntegerField(f).Value)
        else
          SetProperty(Obj, Prop.Name, TValue.From<Variant>(f.Value))
      end;
    end;
end;

class procedure TdormUtils.CopyObject(SourceObj, TargetObj: TObject);
var
  _ARttiType: TRttiType;
  Field: TRttiField;
  master, cloned: TObject;
  Src: TObject;
  sourceStream: TStream;
  SavedPosition: Int64;
  targetStream: TStream;
  targetCollection: TdormCollection;
  sourceCollection: TdormCollection;
  I: Integer;
  sourceObject: TObject;
  targetObject: TObject;
begin
  if not Assigned(TargetObj) then
    exit;

  _ARttiType := ctx.GetType(SourceObj.ClassType);
  cloned := TargetObj;
  master := SourceObj;
  for Field in _ARttiType.GetFields do
  begin
    if not Field.FieldType.IsInstance then
      Field.SetValue(cloned, Field.GetValue(master))
    else
    begin
      Src := Field.GetValue(SourceObj).AsObject;
      if Src is TStream then
      begin
        sourceStream := TStream(Src);
        SavedPosition := sourceStream.Position;
        sourceStream.Position := 0;
        if Field.GetValue(cloned).IsEmpty then
        begin
          targetStream := TMemoryStream.Create;
          Field.SetValue(cloned, targetStream);
        end
        else
          targetStream := Field.GetValue(cloned).AsObject as TStream;
        targetStream.Position := 0;
        targetStream.CopyFrom(sourceStream, sourceStream.Size);
        targetStream.Position := SavedPosition;
        sourceStream.Position := SavedPosition;
      end
      else if Src is TdormCollection then
      begin
        sourceCollection := TdormCollection(Src);
        if Field.GetValue(cloned).IsEmpty then
        begin
          targetCollection := TdormCollection.Create;
          Field.SetValue(cloned, targetCollection);
        end
        else
          targetCollection := Field.GetValue(cloned)
            .AsObject as TdormCollection;
        for I := 0 to sourceCollection.Count - 1 do
        begin
          targetCollection.Add(TdormUtils.Clone(sourceCollection[I]));
        end;
      end
      else
      begin
        sourceObject := Src;

        if Field.GetValue(cloned).IsEmpty then
        begin
          targetObject := TdormUtils.Clone(sourceObject);
          Field.SetValue(cloned, targetObject);
        end
        else
        begin
          targetObject := Field.GetValue(cloned).AsObject;
          TdormUtils.CopyObject(sourceObject, targetObject);
        end;
      end;
    end;
  end;
end;

class function TdormUtils.CreateObject(ARttiType: TRttiType): TObject;
var
  Method: TRttiMethod;
  metaClass: TClass;
begin
  { First solution, clear and slow }
  metaClass := nil;
  for Method in ARttiType.GetMethods do
    if Method.HasExtendedInfo and Method.IsConstructor then
      if Length(Method.GetParameters) = 0 then
      begin
        metaClass := ARttiType.AsInstance.MetaclassType;
        Break;
      end;
  if Assigned(metaClass) then
    Result := Method.Invoke(metaClass, []).AsObject
  else
    raise EdormException.Create('Cannot find a propert constructor for ' +
      ARttiType.ToString);

  { Second solution, dirty and fast }
  // Result := TObject(ARttiType.GetMethod('Create')
  // .Invoke(ARttiType.AsInstance.MetaclassType, []).AsObject);
end;

class function TdormUtils.Clone(Obj: TObject): TObject;
var
  _ARttiType: TRttiType;
  Field: TRttiField;
  master, cloned: TObject;
  Src: TObject;
  sourceStream: TStream;
  SavedPosition: Int64;
  targetStream: TStream;
  targetCollection: TdormCollection;
  sourceCollection: TdormCollection;
  I: Integer;
  sourceObject: TObject;
  targetObject: TObject;
begin
  Result := nil;
  if not Assigned(Obj) then
    exit;

  _ARttiType := ctx.GetType(Obj.ClassType);
  cloned := CreateObject(_ARttiType);
  master := Obj;
  for Field in _ARttiType.GetFields do
  begin
    if not Field.FieldType.IsInstance then
      Field.SetValue(cloned, Field.GetValue(master))
    else
    begin
      Src := Field.GetValue(Obj).AsObject;
      if Src is TStream then
      begin
        sourceStream := TStream(Src);
        SavedPosition := sourceStream.Position;
        sourceStream.Position := 0;
        if Field.GetValue(cloned).IsEmpty then
        begin
          targetStream := TMemoryStream.Create;
          Field.SetValue(cloned, targetStream);
        end
        else
          targetStream := Field.GetValue(cloned).AsObject as TStream;
        targetStream.Position := 0;
        targetStream.CopyFrom(sourceStream, sourceStream.Size);
        targetStream.Position := SavedPosition;
        sourceStream.Position := SavedPosition;
      end
      else if Src is TdormCollection then
      begin
        sourceCollection := TdormCollection(Src);
        if Field.GetValue(cloned).IsEmpty then
        begin
          targetCollection := TdormCollection.Create;
          Field.SetValue(cloned, targetCollection);
        end
        else
          targetCollection := Field.GetValue(cloned)
            .AsObject as TdormCollection;
        for I := 0 to sourceCollection.Count - 1 do
        begin
          targetCollection.Add(TdormUtils.Clone(sourceCollection[I]));
        end;
      end
      else
      begin
        sourceObject := Src;

        if Field.GetValue(cloned).IsEmpty then
        begin
          targetObject := TdormUtils.Clone(sourceObject);
          Field.SetValue(cloned, targetObject);
        end
        else
        begin
          targetObject := Field.GetValue(cloned).AsObject;
          TdormUtils.CopyObject(sourceObject, targetObject);
        end;
        Field.SetValue(cloned, targetObject);
      end;
    end;

  end;
  Result := cloned;
end;

end.
