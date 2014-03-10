program DORM_HelloWorld_ObjVersion;

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


procedure ObjVersionDEMO;
var
  dormSession: TSession;
  Customer: TCustomerV;
  id: Integer;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    Customer := TCustomerV.Create;
    WriteLn('Version: ', Customer.ObjVersion);
    Customer.Name := 'Daniele Teti Inc.';
    Customer.Address := 'Via Roma, 16';
    Customer.EMail := 'daniele@danieleteti.it';
    Customer.CreatedAt := date;
    dormSession.Persist(Customer);
    WriteLn('Version: ', Customer.ObjVersion);

    Customer.Name := 'Daniele Teti Inc.';
    Customer.ObjStatus := osDirty;
    dormSession.Persist(Customer);
    WriteLn('Version: ', Customer.ObjVersion);

    Customer.Address := 'Piazza Roma, 12';
    Customer.ObjStatus := osDirty;
    dormSession.Persist(Customer);
    WriteLn('Version: ', Customer.ObjVersion);

  finally
    dormSession.Free;
  end;
end;

procedure ObjVersionConcurrentTransactionsDEMO;
var
  dormSession, dormSession1, dormSession2: TSession;
  Customer, C1, C2: TCustomerV;
  id: Integer;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    Customer := TCustomerV.Create;
    Customer.Name := 'Daniele Teti Inc.';
    Customer.Address := 'Via Roma, 16';
    Customer.EMail := 'daniele@danieleteti.it';
    Customer.CreatedAt := date;
    dormSession.Persist(Customer);
    id := Customer.id;
    WriteLn('Version: ', Customer.ObjVersion);
    dormSession.Commit(true);
  finally
    dormSession.Free;
  end;

  dormSession1 := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    dormSession2 := TSession.CreateConfigured(
      TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
    try
      // Two users gets the same record
      C1 := dormSession1.Load<TCustomerV>(id);
      dormSession1.Commit;

      C2 := dormSession2.Load<TCustomerV>(id);
      dormSession2.Commit;

      // First user update the object and save it
      C1.Name := 'John Doe';
      C1.ObjStatus := osDirty;
      dormSession1.Persist(C1);
      dormSession1.Commit;

      // The second user try to do the same
      C2.Name := 'Jane Doe';
      C2.ObjStatus := osDirty;
      dormSession2.Persist(C2); // raise EDORMLockingException
    finally
      dormSession2.Free;
    end;
  finally
    dormSession.Free;
  end;
end;

begin
  ObjVersionDEMO;
  // ObjVersionConcurrentTransactionsDEMO;
  ReadLn;

end.
