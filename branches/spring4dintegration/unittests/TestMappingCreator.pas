unit TestMappingCreator;

interface

uses
  TestFramework, dorm.MappingCreator, dorm.MappingCreator.Strategy.Firebird,
  dorm.Configuration, BaseTestCase, superobject;

type
  TTestMappingCreator = class(TBaseTestCase)
  protected
    function ParseAsJson(Value: String): ISuperObject;
  published
    procedure TestTableNameToClassName;
{$IFDEF FIREBIRD_UIB_STRATEGY}
    procedure TestFirebirdUIB;
{$ENDIF}
  end;

implementation

uses
  classes, SysUtils, ioutils, dorm.MappingCreator.Strategy, generics.defaults;

{ TTestMappingCreator }

function TTestMappingCreator.ParseAsJson(Value: String): ISuperObject;
begin
  Result := SO(Value);
  if Result = nil then
    raise Exception.Create('Invalid json');
end;

{$IFDEF FIREBIRD_UIB_STRATEGY}

procedure TTestMappingCreator.TestFirebirdUIB;
var
  mc: TMappingCreator;
  ss: TStringStream;
  S: ISuperObject;
begin
  mc := TMappingCreator.Create(TFirebirdMappingCreatorStrategy.Create);
  try
    mc.GetCurrentStrategy.SetConfiguration(TdormConfiguration.Create
      (TStreamReader.Create(GetDORMConfigFileName), etTest));
    ss := TStringStream.Create;
    try
      CheckTrue(mc.Execute(ss));
      S := ParseAsJson(ss.DataString);
      CheckNotNull(S);
      // Used only to format the output file
      S.SaveTo('test.mapping.json', true);
      S := nil;
    finally
      ss.free;
    end;
  finally
    mc.free;
  end;
end;
{$ENDIF}


procedure TTestMappingCreator.TestTableNameToClassName;
begin
  // CheckEquals('TCars', TMappingCreatorStrategy.TableNameToClassName('CARS'));
  // CheckEquals('TPeople', TMappingCreatorStrategy.TableNameToClassName
  // ('people'));
  // CheckEquals('TInvoiceDetails', TMappingCreatorStrategy.TableNameToClassName
  // ('invoice details'));
  // CheckEquals('TInvoiceDetails', TMappingCreatorStrategy.TableNameToClassName
  // ('invoice_details'));
  // CheckEquals('TInvoiceDetails', TMappingCreatorStrategy.TableNameToClassName
  // ('_invoice_details_'));
end;

initialization

RegisterTest(TTestMappingCreator.Suite);

end.
