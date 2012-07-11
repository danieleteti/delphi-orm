unit dorm.CodeGenerator.Delphi;

interface

uses dorm.CodeGenerator, Data.DBXJSON, Classes;

type
  TDelphiCodeGenerator = class(TCodeGenerator)
  private
    FInterfaceIndentation, FImplementationIndentation: String;
    FInterfaceStream, FImplementationStream: TStringStream;
    FInterfaceWriter: TStreamWriter;
    FImplementationWriter: TStreamWriter;
    procedure InitStreams;
    procedure ReleaseStreams;
    procedure WriteClassDeclaration(ClazzName: String;
      AClassDefinition: TJSONObject);
    function dormToDelphiType(const dormTypeName: String): string;
  protected
    procedure InterfaceIndent;
    procedure InterfaceOutdent;
    procedure ImplementationIndent;
    procedure ImplementationOutdent;
    procedure WriteInterfaceLine(const ALine: String);
    procedure WriteImplementationLine(const ALine: String);
    function InternalExecute(AMapping: TJSONObject;
      AOutputStream: TTextWriter): Boolean; override;
  end;

implementation

uses
  System.SysUtils;

{ TDelphiCodeGenerator }

function TDelphiCodeGenerator.dormToDelphiType(
  const dormTypeName: String): string;
begin
  if dormTypeName = 'integer' then
    Exit('Integer');
  if dormTypeName = 'string' then
    Exit('String');
  if dormTypeName = 'date' then
    Exit('TDate');
  if dormTypeName = 'datetime' then
    Exit('TDateTime');
  if dormTypeName = 'blob' then
    Exit('TStream');
  raise Exception.CreateFmt('Unknown type %s', [dormTypeName]);
end;

procedure TDelphiCodeGenerator.InterfaceIndent;
begin
  FInterfaceIndentation := FInterfaceIndentation + '  ';
end;

procedure TDelphiCodeGenerator.InterfaceOutdent;
begin
  if Length(FInterfaceIndentation) > 2 then
    FInterfaceIndentation := StringOfChar(' ',
      Length(FInterfaceIndentation) - 2)
  else
    FInterfaceIndentation := '';
end;

procedure TDelphiCodeGenerator.InitStreams;
begin
  FInterfaceIndentation := '';
  FImplementationIndentation := '';
  FInterfaceStream := TStringStream.Create;
  FImplementationStream := TStringStream.Create;
  FInterfaceWriter := TStreamWriter.Create(FInterfaceStream, TEncoding.ASCII);
  FInterfaceWriter.OwnStream;
  FImplementationWriter := TStreamWriter.Create(FImplementationStream,
    TEncoding.ASCII);
  FImplementationWriter.OwnStream;
  WriteInterfaceLine('interface');
  WriteInterfaceLine('');
  WriteImplementationLine('implementation');
  WriteImplementationLine('');
end;

procedure TDelphiCodeGenerator.ImplementationIndent;
begin

end;

procedure TDelphiCodeGenerator.ImplementationOutdent;
begin

end;

function TDelphiCodeGenerator.InternalExecute(AMapping: TJSONObject;
  AOutputStream: TTextWriter): Boolean;
var
  pair: TJSONPair;
  ClazzName: string;
  ClassDefinition: TJSONObject;
begin
  InitStreams;
  WriteInterfaceLine('type');
  for pair in AMapping do
  begin
    InterfaceIndent;
    ClazzName := pair.JsonString.Value;
    ClassDefinition := AMapping.Get(ClazzName).JsonValue as TJSONObject;
    WriteClassDeclaration(ClazzName, ClassDefinition);
    InterfaceOutdent;
  end;

  AOutputStream.WriteLine('unit BusinnessObjects;');
  AOutputStream.WriteLine;
  AOutputStream.Write(FInterfaceStream.DataString);
  AOutputStream.Write(FImplementationStream.DataString);
  AOutputStream.WriteLine;
  AOutputStream.WriteLine('initialization');
  AOutputStream.WriteLine;
  AOutputStream.WriteLine('finalization');
  AOutputStream.WriteLine;
  AOutputStream.WriteLine('end.');
  ReleaseStreams;
end;

procedure TDelphiCodeGenerator.ReleaseStreams;
begin
  FInterfaceWriter.Free;
  FImplementationWriter.Free;
end;

procedure TDelphiCodeGenerator.WriteClassDeclaration(ClazzName: String;
  AClassDefinition: TJSONObject);
var
  Fields: TJSONArray;
  Field: TJSONObject;
  i: Integer;
  PkField: TJSONObject;
begin
  WriteInterfaceLine('');
  Fields := AClassDefinition.Get('fields').JsonValue as TJSONArray;
  WriteInterfaceLine(ClazzName + ' = class(TObject)');
  InterfaceIndent;

  if assigned(AClassDefinition.Get('id')) then
  begin
    PkField := AClassDefinition.Get('id').JsonValue as TJSONObject;
    WriteInterfaceLine('//primary key');
    WriteInterfaceLine(PkField.Get('name').JsonValue.Value +
      ': ' +
      dormToDelphiType(PkField.Get('field_type').JsonValue.Value) + ';');
  end;

  WriteInterfaceLine('//fields');
  for i := 0 to Fields.Size - 1 do
  begin
    Field := Fields.Get(i) as TJSONObject;
    WriteInterfaceLine(Field.Get('name').JsonValue.Value +
      ': ' +
      dormToDelphiType(Field.Get('field_type').JsonValue.Value) + ';');
  end;
  InterfaceOutdent;
  WriteInterfaceLine('end;');
end;

procedure TDelphiCodeGenerator.WriteImplementationLine(const ALine: String);
begin
  FImplementationWriter.WriteLine(FImplementationIndentation + ALine);
end;

procedure TDelphiCodeGenerator.WriteInterfaceLine(const ALine: String);
begin
  FInterfaceWriter.WriteLine(FInterfaceIndentation + ALine);
end;

end.
