unit dorm.Utils;

interface

uses
  RTTI,
  DB;

type
  TdormUtils = class sealed
  private
    class var ctx: TRttiContext;
  public
    class procedure SetProperty(Obj: TObject; const PropertyName: string;
      const Value: TValue); static;
    class procedure ObjectToDataSet(Obj: TObject; Field: TField; var Value: Variant);
    class procedure DatasetToObject(Dataset: TDataset; Obj: TObject);
    class function GetProperty(Obj: TObject; const PropertyName: string): TValue;
    class function GetField(Obj: TObject; const PropertyName: string): TValue;
    class procedure SetField(Obj: TObject; const PropertyName: string; const Value: TValue);
    class function Clone(Obj: TObject): TObject; static;
    class function CreateObject(ARttiType: TRttiType): TObject; static;
  end;

function FieldFor(const PropertyName: string): string; inline;

implementation

uses
  SysUtils,
  Classes;

function FieldFor(const PropertyName: string): string; inline;
begin
  Result := 'F' + PropertyName;
end;

class function TdormUtils.GetField(Obj: TObject;
  const PropertyName: string): TValue;
var
  prop: TRttiField;
  rtti_type: TRttiType;
begin
  rtti_type := ctx.GetType(Obj.ClassType);
  if not assigned(rtti_type) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [rtti_type.ToString]);
  prop := rtti_type.GetField(FieldFor(PropertyName));
  if not assigned(prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [rtti_type.ToString, PropertyName]);
  Result := prop.GetValue(Obj)
end;

class function TdormUtils.GetProperty(Obj: TObject;
  const PropertyName: string): TValue;
var
  prop: TRttiProperty;
  rtti_type: TRttiType;
begin
  rtti_type := ctx.GetType(Obj.ClassType);
  if not assigned(rtti_type) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [rtti_type.ToString]);
  prop := rtti_type.GetProperty(PropertyName);
  if not assigned(prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [rtti_type.ToString, PropertyName]);
  if prop.IsReadable then
    Result := prop.GetValue(Obj)
  else
    raise Exception.CreateFmt('Property is not readable [%s.%s]', [rtti_type.ToString, PropertyName]);
end;

class procedure TdormUtils.SetField(Obj: TObject; const PropertyName: string;
  const Value: TValue);
var
  prop: TRttiField;
  rtti_type: TRttiType;
begin
  rtti_type := ctx.GetType(Obj.ClassType);
  if not assigned(rtti_type) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [rtti_type.ToString]);
  prop := rtti_type.GetField(FieldFor(PropertyName));
  if not assigned(prop) then
    raise Exception.CreateFmt('Cannot get RTTI for field [%s.%s]', [rtti_type.ToString, PropertyName]);
  prop.SetValue(Obj, Value);
end;

class procedure TdormUtils.SetProperty(Obj: TObject;
  const PropertyName: string; const Value: TValue);
var
  prop: TRttiProperty;
  rtti_type: TRttiType;
begin
  rtti_type := ctx.GetType(Obj.ClassType);
  if not assigned(rtti_type) then
    raise Exception.CreateFmt('Cannot get RTTI for type [%s]', [rtti_type.ToString]);
  prop := rtti_type.GetProperty(PropertyName);
  if not assigned(prop) then
    raise Exception.CreateFmt('Cannot get RTTI for property [%s.%s]', [rtti_type.ToString, PropertyName]);
  if prop.IsWritable then
    prop.SetValue(Obj, Value)
  else
    raise Exception.CreateFmt('Property is not writeable [%s.%s]', [rtti_type.ToString, PropertyName]);
end;

class procedure TdormUtils.ObjectToDataSet(Obj: TObject; Field: TField; var Value: Variant);
begin
  Value := GetProperty(Obj, Field.FieldName).AsVariant;
end;

class procedure TdormUtils.DatasetToObject(Dataset: TDataset; Obj: TObject);
var
  rtti_type: TRttiType;
  props: TArray<TRttiProperty>;
  prop: TRttiProperty;
  f: TField;
begin
  rtti_type := ctx.GetType(Obj.ClassType);
  props := rtti_type.GetProperties;
  for prop in props do
    if not SameText(prop.Name, 'ID') then
    begin
      f := Dataset.FindField(prop.Name);
      if assigned(f) and not f.ReadOnly then
      begin
        if f is TIntegerField then
          SetProperty(Obj, prop.Name, TIntegerField(f).Value)
        else
          SetProperty(Obj, prop.Name, TValue.From<Variant>(f.Value))
      end;
    end;
end;

class function TdormUtils.CreateObject(ARttiType: TRttiType): TObject;
begin
  Result := TObject(ARttiType.GetMethod('Create').Invoke(ARttiType.AsInstance.MetaclassType, []).AsObject);
end;

class function TdormUtils.Clone(Obj: TObject): TObject;
var
  _rtti_type: TRttiType;
  Field: TRttiField;
  master, cloned: TObject;
  Src: TObject;
  sourceStream: TStream;
  SavedPosition: Int64;
  targetStream: TStream;
begin
  result:= nil;
  if not assigned(obj) then exit;

  _rtti_type := ctx.GetType(Obj.ClassType);
  cloned := CreateObject(_rtti_type);
  master := Obj;
  for Field in _rtti_type.GetFields do
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
      end;
    end;

  end;
  Result := cloned;
end;

end.
