program DORM_HelloWorld;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.Classes,
  dorm,
  dorm.commons,
  dorm.loggers,
  BObjectsU in '..\..\Commons\BObjectsU.pas',
  RandomUtilsU in '..\..\Commons\RandomUtilsU.pas';

  {$IFDEF LINK_SQLSERVERFIREDAC_ADAPTER}
  const CONFIG_FILE = '..\..\dorm_sqlserver_firedac.conf';
  {$ENDIF}
  {$IFNDEF LINK_SQLSERVERFIREDAC_ADAPTER}
  const CONFIG_FILE = '..\..\dorm.conf';
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
  SimpleInsert;
  //SimpleCRUD;
  ReadLn;

end.
