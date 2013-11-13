program DORM_HelloWorld_Events;

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

procedure SimpleCRUD;
var
  dormSession: TSession;
  Customer: TCustomerOS;
  id: Integer;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create('..\..\dorm.conf'), TdormEnvironment.deDevelopment);
  try
    Customer := TCustomerVal.Create;
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
  SimpleCRUD;
  ReadLn;

end.
