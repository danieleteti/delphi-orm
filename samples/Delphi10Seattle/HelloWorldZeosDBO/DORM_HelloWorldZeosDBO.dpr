program DORM_HelloWorldZeosDBO;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.Classes,
  dorm,
  dorm.commons,
  dorm.loggers,
  BObjectsU in '..\..\Commons\BObjectsU.pas',
  RandomUtilsU in '..\..\Commons\RandomUtilsU.pas',
  dorm.adapter.ZeosDBO.BaseAdapter in '..\..\..\source\dorm.adapter.ZeosDBO.BaseAdapter.pas',
  dorm.adapter.ZeosDBO.Facade in '..\..\..\source\dorm.adapter.ZeosDBO.Facade.pas',
  dorm.adapter.ZeosDBO.Firebird in '..\..\..\source\dorm.adapter.ZeosDBO.Firebird.pas';


{$IFDEF LINK_SQLSERVERFIREDAC_ADAPTER}
const
  CONFIG_FILE = '..\..\dorm_sqlserver_firedac.conf';
{$ENDIF}
{$IFNDEF LINK_SQLSERVERFIREDAC_ADAPTER}


const
  CONFIG_FILE = '..\..\dorm.conf';
{$ENDIF}


procedure SimpleInsert;
var
  dormSession: TSession;
  Customer: TCustomer;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    Customer := TCustomer.Create;
    Customer.Name := 'Daniele Teti Inc.';
    Customer.Address := 'Via Roma, 16';
    Customer.EMail := 'daniele@danieleteti.it';
    Customer.CreatedAt := date;
    dormSession.Insert(Customer);
    Customer.Free;
  finally
    dormSession.Free;
  end;
end;

procedure SimpleCRUD;
var
  dormSession: TSession;
  Customer: TCustomer;
  id: Integer;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    Customer := TCustomer.Create;
    Customer.Name := 'Daniele Teti Inc.';
    Customer.Address := 'Via Roma, 16';
    Customer.EMail := 'daniele@danieleteti.it';
    Customer.CreatedAt := date;
    dormSession.Insert(Customer);
    id := Customer.id;
    Customer.Free;

    // update data
    Customer := dormSession.Load<TCustomer>(id);
    Customer.Address := 'Piazza Roma,1';
    dormSession.Update(Customer);
    Customer.Free;

    // delete data
    Customer := dormSession.Load<TCustomer>(id);
    dormSession.Delete(Customer);
    Customer.Free;

  finally
    dormSession.Free;
  end;
end;

begin
  Write('Press return to execute an insert...');
  ReadLn;
  SimpleInsert;
  Write('Press return to execute a complete CRUD...');
  ReadLn;
  SimpleCRUD;
  Write('Press return to terminate...');
  ReadLn;

end.
