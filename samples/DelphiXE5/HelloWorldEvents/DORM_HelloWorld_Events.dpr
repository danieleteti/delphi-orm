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

  {$IFDEF LINK_SQLSERVERFIREDAC_ADAPTER}
  const CONFIG_FILE = '..\..\dorm_sqlserver_firedac.conf';
  {$ENDIF}
  {$IFNDEF LINK_SQLSERVERFIREDAC_ADAPTER}
  const CONFIG_FILE = '..\..\dorm.conf';
  {$ENDIF}

procedure SimpleCRUD;
var
  dormSession: TSession;
  Customer: TCustomerVal;
  id: Integer;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    Customer := TCustomerVal.Create;
    Customer.Name := 'Daniele Teti Inc.';
    Customer.Address := 'Via Roma, 16';
    Customer.EMail := 'daniele@danieleteti.it';
    Customer.CreatedAt := date;

    dormSession.Persist(Customer);
    id := Customer.id;
    Customer.Free;

    Customer := dormSession.Load<TCustomerVal>(id);
    Writeln('Name:      ', Customer.Name);
    Writeln('Address:   ', Customer.Address);
    Writeln('EMail:     ', Customer.EMail);
    Writeln('CreatedAt: ', DateToStr(Customer.CreatedAt));
    Customer.Free;
  finally
    dormSession.Free;
  end;
end;

begin
  SimpleCRUD;
  ReadLn;

end.
