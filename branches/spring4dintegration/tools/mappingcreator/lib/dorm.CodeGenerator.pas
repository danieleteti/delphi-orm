unit dorm.CodeGenerator;

interface

uses
  Classes, Data.DBXJSON;

type
  TCodeGenerator = class abstract
  protected
    function InternalExecute(AMapping: TJSONObject;
      AOutputStream: TTextWriter): boolean; virtual; abstract;
  public
    constructor Create; virtual;
    function Execute(AMapping: TTextReader; AOutputFile: String): boolean;
  end;

implementation

uses
  System.SysUtils;

{ TCodeGenerator }

constructor TCodeGenerator.Create;
begin
  inherited;
end;

function TCodeGenerator.Execute(AMapping: TTextReader;
  AOutputFile: String): boolean;
var
  json: TJSONObject;
  sw: TStreamWriter;
begin
  sw := TStreamWriter.Create(TFileStream.Create(AOutputFile, fmCreate),
    TEncoding.ASCII);
  try
    sw.OwnStream;
    json := TJSONObject.ParseJSONValue(AMapping.ReadToEnd) as TJSONObject;
    try
      InternalExecute(json, sw);
    finally
      json.Free;
    end;
  finally
    sw.Free;
  end;
end;

end.
