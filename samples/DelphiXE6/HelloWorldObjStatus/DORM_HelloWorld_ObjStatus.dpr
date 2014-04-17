program DORM_HelloWorld_ObjStatus;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.Classes,
  dorm,
  dorm.commons,
  dorm.loggers,
  BObjectsU in '..\..\Commons\BObjectsU.pas',
  dorm.ObjectStatus,
  RandomUtilsU in '..\..\Commons\RandomUtilsU.pas';

{$IFDEF LINK_SQLSERVERFIREDAC_ADAPTER}

const
  CONFIG_FILE = '..\..\dorm_sqlserver_firedac.conf';
{$ENDIF}
{$IFNDEF LINK_SQLSERVERFIREDAC_ADAPTER}

const
  CONFIG_FILE = '..\..\dorm.conf';
{$ENDIF}


procedure SimpleCRUD;
var
  dormSession: TSession;
  Customer: TCustomerOS;
  id: Integer;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    Customer := TCustomerOS.Create;
    Customer.Name := 'Daniele Teti Inc.';
    Customer.Address := 'Via Roma, 16';
    Customer.EMail := 'daniele@danieleteti.it';
    Customer.CreatedAt := date;

    // objectstatus knows what Persist should do...
    // you dont have to specify Insert/Update/Delete
    // ObjStatus of new created object is osDirty
    dormSession.Persist(Customer);
    id := Customer.id;
    Customer.Free;

    // now, update the data
    Customer := dormSession.Load<TCustomerOS>(id);
    // ObjStatus = osClean

    Customer.Address := 'Piazza Roma,1';
    Customer.ObjStatus := osDirty;
    // OID is <> 0 the ObjStatus = osDirty
    // so the PODO will be updated
    dormSession.Persist(Customer);
    Customer.Free;

    // delete data
    Customer := dormSession.Load<TCustomerOS>(id);
    Customer.ObjStatus := osDeleted;
    dormSession.Persist(Customer);
    Customer.Free;

  finally
    dormSession.Free;
  end;
end;

begin
  Write('Press return to execute CRUD using dorm'' ObjStatus...');
  ReadLn;

  SimpleCRUD;
  Write('Press return to terminate');
  ReadLn;

end.
