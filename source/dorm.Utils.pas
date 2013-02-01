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

unit dorm.Utils;

interface

uses
  RTTI,
  DB,
  Generics.Collections;

type
  TdormUtils = class sealed
  public
    class var ctx: TRttiContext;

  public
    class function MethodCall(AObject: TObject; AMethodName: string;
      AParameters: array of TValue): TValue;
    class procedure SetProperty(Obj: TObject; const PropertyName: string;
      const Value: TValue); static;
    class function GetFieldType(AProp: TRttiProperty): string;
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
    class function GetAttribute<T: TCustomAttribute>(const Obj: TRttiObject)
      : T; overload;
    class function GetAttribute<T: TCustomAttribute>(const Obj: TRttiType)
      : T; overload;
    class function HasAttribute<T: TCustomAttribute>
      (const Obj: TRttiObject): Boolean;
    class function EqualValues(source, destination: TValue): Boolean;
  end;

function FieldFor(const PropertyName: string): string; inline;

implementation

uses
  SysUtils,
  Classes,
  TypInfo,
  dorm.Commons,
  dorm.Collections;

class function TdormUtils.MethodCall(AObject: TObject; AMethodName: string;
  AParameters: array of TValue): TValue;
var
  m: TRttiMethod;
begin
  m := ctx.GetType(AObject.ClassInfo).GetMethod(AMethodName);
  if Assigned(m) then
    Result := m.Invoke(AObject, AParameters)
  else
    raise EdormException.CreateFmt('Cannot find method "%s" in the object',
      [AMethodName]);
end;

function FieldFor(const PropertyName: string): string; inline;
begin
  Result := 'F' + PropertyName;
end;

class function TdormUtils.GetAttribute<T>(const Obj: TRttiObject): T;
var
  Attr: TCustomAttribute;
begin
  Result := nil;
  for Attr in Obj.GetAttributes do
  begin
    if Attr.ClassType.InheritsFrom(T) then
      Exit(T(Attr));
  end;
end;

class function TdormUtils.GetAttribute<T>(const Obj: TRttiType): T;
var
  Attr: TCustomAttribute;
begin
  Result := nil;
  for Attr in Obj.GetAttributes do
  begin
    if Attr.ClassType.InheritsFrom(T) then
      Exit(T(Attr));
  end;
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

class function TdormUtils.HasAttribute<T>(const Obj: TRttiObject): Boolean;
begin
  Result := Assigned(GetAttribute<T>(Obj));
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
    begin
      if Prop.IsWritable then
        Prop.SetValue(Obj, Value)
    end
    else
      raise Exception.CreateFmt
        ('Cannot get RTTI for field or property [%s.%s]',
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

class function TdormUtils.GetFieldType(AProp: TRttiProperty): string;
var
  _PropInfo: PTypeInfo;
begin
  _PropInfo := AProp.PropertyType.Handle;
  if _PropInfo.Kind in [tkString, tkWString, tkChar, tkWChar, tkLString,
    tkUString] then
    Result := 'string'
  else if _PropInfo.Kind in [tkInteger, tkInt64] then
    Result := 'integer'
  else if _PropInfo = TypeInfo(TDate) then
    Result := 'date'
  else if _PropInfo = TypeInfo(TDateTime) then
    Result := 'datetime'
  else if _PropInfo = TypeInfo(Currency) then
    Result := 'decimal'
  else if _PropInfo.Kind = tkFloat then
    Result := 'float'
  else if (_PropInfo.Kind = tkEnumeration) and (_PropInfo.Name = 'Boolean') then
    Result := 'boolean'
  else if AProp.PropertyType.IsInstance and
    AProp.PropertyType.AsInstance.MetaclassType.InheritsFrom
    (TStream) then
    Result := 'blob'
  else
    Result := EmptyStr;
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

class function TdormUtils.EqualValues(source, destination: TValue): Boolean;
begin
  // Really UniCodeCompareStr (Annoying VCL Name for backwards compatablity)
  Result := AnsiCompareStr(source.ToString, destination.ToString) = 0;
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
  targetCollection: TObjectList<TObject>;
  sourceCollection: TObjectList<TObject>;
  I: Integer;
  sourceObject: TObject;
  targetObject: TObject;
begin
  if not Assigned(TargetObj) then
    Exit;

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
      else if Src is TObjectList<TObject> then
      begin
        sourceCollection := TObjectList<TObject>(Src);
        if Field.GetValue(cloned).IsEmpty then
        begin
          targetCollection := TObjectList<TObject>.Create;
          Field.SetValue(cloned, targetCollection);
        end
        else
          targetCollection := Field.GetValue(cloned)
            .AsObject as TObjectList<TObject>;
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
  Method := nil;
  for Method in ARttiType.GetMethods do
    if Method.HasExtendedInfo and Method.IsConstructor then
      if length(Method.GetParameters) = 0 then
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
  targetCollection: TObjectList<TObject>;
  sourceCollection: TObjectList<TObject>;
  I: Integer;
  sourceObject: TObject;
  targetObject: TObject;
begin
  Result := nil;
  if not Assigned(Obj) then
    Exit;

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
      else if Src is TObjectList<TObject> then
      begin
        sourceCollection := TObjectList<TObject>(Src);
        if Field.GetValue(cloned).IsEmpty then
        begin
          targetCollection := TObjectList<TObject>.Create;
          Field.SetValue(cloned, targetCollection);
        end
        else
          targetCollection := Field.GetValue(cloned)
            .AsObject as TObjectList<TObject>;
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

{ TListDuckTyping }

end.
