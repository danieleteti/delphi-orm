unit dorm.MappingCreator;

interface

uses
  dorm.MappingCreator.Strategy, System.Generics.Collections, System.Classes;

const
  VERSION = '0.1';

type
  TMappingCreator = class
  private
    FMappingStrategy: TMappingCreatorStrategy;
  protected
    // const
    // TEMPLATE_MAPPING_PLACEHOLDER = '$$MAPPING$$';
    // procedure MergeWithTemplate(AOutputStream: TStream);
  public
    constructor Create(MappingStrategy: TMappingCreatorStrategy);
    destructor Destroy; override;
    function GetCurrentStrategy: TMappingCreatorStrategy;
    function Execute(AOutputStream: TStream;
      ACustomDictionary: TDictionary<string, string>): Boolean; overload;
    function Execute(AOutputStream: TStream): Boolean; overload;
    function Warnings: TList<string>;
    function Errors: TList<string>;
  end;

implementation

{ TMappingCreator }

uses dorm.Utils, System.SysUtils, ioutils;

constructor TMappingCreator.Create(MappingStrategy: TMappingCreatorStrategy);
begin
  inherited Create;
  FMappingStrategy := MappingStrategy;
end;

destructor TMappingCreator.Destroy;
begin
  FMappingStrategy.Free;
  inherited;
end;

function TMappingCreator.Errors: TList<string>;
begin
  Result := FMappingStrategy.Errors;
end;

// procedure TMappingCreator.MergeWithTemplate(AOutputStream: TStream);
// var
// mp, Template: string;
// sr: TStreamReader;
// sw: TStreamWriter;
// begin
// mp := TdormUtils.StreamToText(AOutputStream);
// Template := GetTemplate;
// mp := StringReplace(Template, TEMPLATE_MAPPING_PLACEHOLDER, mp, []);
// AOutputStream.Size := 0;
// sw := TStreamWriter.Create(AOutputStream);
// try
// sw.Write(mp);
// finally
// sw.Free;
// end;
// AOutputStream.Position := 0;
// end;

function TMappingCreator.Execute(AOutputStream: TStream): Boolean;
begin
  Result := FMappingStrategy.Execute(AOutputStream);
  // if Result then
  // MergeWithTemplate(AOutputStream);
end;

function TMappingCreator.Execute(AOutputStream: TStream;
  ACustomDictionary: TDictionary<string, string>): Boolean;
begin
  Result := FMappingStrategy.Execute(AOutputStream, ACustomDictionary);
end;

function TMappingCreator.GetCurrentStrategy: TMappingCreatorStrategy;
begin
  Result := FMappingStrategy;
end;

function TMappingCreator.Warnings: TList<string>;
begin
  Result := FMappingStrategy.Warnings;
end;

end.
