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


const
  CONFIG_FILE = '..\..\dorm_sqlserver_firedac.conf';
{$ENDIF}

{$IFNDEF LINK_SQLSERVERFIREDAC_ADAPTER}


const
  CONFIG_FILE = '..\..\dorm.conf';
{$ENDIF}


procedure SimpleCRUD(UseWrongEmail: Boolean);
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

    if UseWrongEmail then
      Customer.EMail := 'danieleteti.it'
    else
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
  Writeln('CRUD with correct email (the email is uppercased by the OnBeforePersist method)');
  SimpleCRUD(false);
  Writeln;
  Writeln('CRUD with incorrect email (an exception is raised by the Validate method)');
  try
    SimpleCRUD(true);
  except
    on E: Exception do
    begin
      Writeln('EXCEPTION RAISED: ' + E.ClassName + ' "' + E.Message + '"');
    end;
  end;
  Writeln;
  Writeln('hit return to terminate demo...');
  ReadLn;

end.
